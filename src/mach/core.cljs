;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:refer-clojure :exclude [import])
  (:require
   [cljs.nodejs :as nodejs]
   [cljs.pprint :as pprint]
   [cljs.reader :as reader]
   [cljs.js :as cljs]
   [lumo.repl :as repl]
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]))

(defonce ^:private st (cljs/empty-state))

;; Sorting

(def toposort (nodejs/require "toposort"))

(defn resolve-target
  "Resolve target key (symbol) matching given target (string) in machfile."
  [machfile target]
  (if (contains? machfile (symbol target))
    (symbol target)
    ;; Else try to search for
    (if-let [target
             (some (fn [x]
                     (let [[k v] x]
                       (when (= target (get v 'product))
                         k)))
                   machfile)]
      (symbol target)
      (throw (ex-info (str "Could not resolve target: " target) {})))))

(defn order [machfile target]
  (map symbol
       (drop 1 ; drop nil
             (js->clj
              (toposort
               (clj->js
                (tree-seq
                 (fn [[_ target]] (-> machfile (get target) (get 'depends)))
                 (fn [[_ target]]
                   (map vector (repeat target) (-> machfile (get target) (get 'depends))))
                 [nil target])))))))

;; References

(defrecord Reference [path])

(defn ^:private read-reference [path]
  (->Reference path))

(reader/register-tag-parser! "ref" read-reference)

(def fs (nodejs/require "fs"))
(def child_process (nodejs/require "child_process"))

(defn spit [f data]
  (println "Writing" f)
  (fs.writeFileSync f data))


(defn file-exists? [f]
  (fs.existsSync f))

(defn last-modified [f]
  (if f
    (if (file-exists? f)
      (.getTime (.-mtime (fs.statSync f)))
      0)
    0))

(defn modified-since? [since f]
  (< since (last-modified f)))

(defn mkdir [path]
  (fs.mkdirSync path))

(defn dir? [f]
  (and
   (file-exists? f)
   (.. fs (lstatSync f) (isDirectory))))

(defn file? [f]
  (.. fs (lstatSync f) (isFile)))

(defn files
  ([dir]
   (when (dir? dir)
     (seq (fs.readdirSync dir))))
  ([]
   (files ".")))

(defn has-suffix [suffix]
  #(.endsWith % suffix))

(defn file-seq [dir]
  (if (fs.existsSync dir)
    (tree-seq
     dir?
     (fn [d] (map (partial str d "/") (seq (fs.readdirSync d))))
     dir)
    []))

(defn modified-since [anchor source]
  (filter
   (partial modified-since? (apply max (conj (map last-modified (filter file? (file-seq anchor))) 0)))
   (filter (comp not mach.core/dir?) (if (coll? source)
                                       (mapcat mach.core/file-seq source)
                                       (mach.core/file-seq source)))))

(defn resolve-symbols [expr scope]
  (postwalk (fn [x]
              (if (symbol? x)
                (if-let [v (get scope x)] v x)
                x))
            expr))

(defn sh [& args]
  (let [args (flatten args)
        _  (apply println "$" args)
        result (.spawnSync child_process
                           (first args)
                           (clj->js (map (comp #(str/replace % "'" "\\'")
                                               #(str/replace % "|" "\\|")) (rest args)
                                         ))
                           #js {"shell" true})]
    (println (str (.-stdout result)))
    (println (str (.-stderr result)))))

(defn ^:private read-shell [vals]
  `(sh ~@vals))

(reader/register-tag-parser! "$" read-shell)

(defn ^:private read-shell-apply [vals]
  `(when (not-empty ~vals) (apply sh ~@vals)))

(reader/register-tag-parser! "$$" read-shell-apply)

(defn ^:private eval-cljs [cljs-file]
  `[~::eval ~cljs-file])

(reader/register-tag-parser! "eval" eval-cljs)

(def ^:private extensions-cache (atom {}))

(defn ^:private add-extension [extensions extension]
  (let [extensions-file (str extension ".mach.edn")]
    (assoc extensions extension (reader/read-string (fs.readFileSync extensions-file "utf-8")))))

(defn import [[target args]]
  (let [[extension k] (str/split (str target) "/")
        extensions (or (get @extensions-cache extension)
                       (get (swap! extensions-cache add-extension extension) extension))]
    (postwalk (fn [v]
                (type v)
                (if (symbol? v)
                  (get args v v) v))
              (get extensions (symbol k)))))

(reader/register-tag-parser! "import" import)

(defn resolve-refs [machfile]
  (fn [x]
    (if (instance? Reference x)
      (get-in machfile (:path x))
      x)))

(defmulti apply-verb
  "Return boolean to indicate if work was done (true) or not (false)"
  (fn [machfile target-name verb] verb))

(defmethod apply-verb :default [machfile target-name verb]
  (throw (ex-info (str "Unknown verb: '" verb "'") {})))

(defn update! [target novelty verb]
  (when (and (get target 'produce)
             (get target 'update!))
    (throw (ex-info "Invalid to have both update! and produce in the same target" {:target target})))

  (let [code (resolve-symbols
              ;; Expression
              (if (map? target)
                (or (get target 'produce)
                    (get target 'update!))
                target)
              ;; Scope
              (if (map? target)
                (merge
                 target
                 ;; Already computed novelty
                 {'novelty `(quote ~novelty)})
                ;; Just a expression, no scope
                {}))]

    (postwalk (fn [x]
                (if (and (vector? x) (= ::eval (first x)))
                  (do
                    (lumo.repl/execute "file" (second x) true true nil)
                    nil)
                  x))
              code)

    (when-let [val (:value (cljs/eval repl/st code identity))]
      ;; Print regardless
      (cond
        (= verb 'print) (println val)
        :otherwise (when (get target 'produce)
                     (when-let [product (get target 'product)]
                       (spit product val)))))

    ;; We did work so return true
    true))

(defmethod apply-verb nil [machfile target-name verb]
  (some identity
        (doall
         (for [target-name (reverse (order machfile target-name))
               :let [target (get machfile target-name)]]
           (do
             (if target
               (let [novelty (when (get target 'novelty)
                               (let [res (cljs/eval
                                          repl/st
                                          (resolve-symbols (get target 'novelty) target)
                                          identity)]
                                 (:value res)))]

                 ;; Call update!
                 (when (or (not (map? target))
                           (and (get target 'update!) (nil? (get target 'novelty)))
                           (true? novelty)
                           (when (seq? novelty) (not-empty novelty)))

                   (update! target novelty verb)))

               ;; Unlikely, already checked this in resolve-target
               (throw (ex-info (str "No target: " target) {}))))))))

;; Run the update (or produce) and print, no deps
(defmethod apply-verb 'update [machfile target-name verb]
  (let [target (get machfile target-name)]
    (if target
      (update! target nil verb)
      (throw (ex-info (str "No target: " target-name) {})))))

;; Print the produce
(defmethod apply-verb 'print [machfile target-name verb]
  (let [target (get machfile target-name)]
    (if target
      (update! target nil verb)
      (throw (ex-info (str "No target: " target-name) {})))))

(defmethod apply-verb 'clean [machfile target verb]
  (doseq [target (order machfile target)
          :let [v (get machfile target)]]
    (if-let [rule (get v 'clean!)]
      ;; If so, call it
      (cljs/eval repl/st (resolve-symbols rule v) identity)
      ;; Otherwise implied policy is to delete declared target files
      (when-let [product (get v 'product)]
        (if (coll? product)
          (if (some dir? product)
            (apply sh "rm" "-rf" product)
            (apply sh "rm" "-f" product))
          (cond
            (dir? product) (sh "rm" "-rf" product)
            (file-exists? product) (sh "rm" "-f" product)
            ;; er? this is overridden later
            :otherwise false)))))
  true)

(defmethod apply-verb 'depends [machfile target-name verb]
  (pprint/pprint
   (order machfile target-name))
  true)

(defmethod apply-verb 'novelty [machfile target-name verb]
  (pprint/pprint
   (when-let [target (get machfile target-name)]
     (when (get target 'novelty)
       (let [res (cljs/eval
                  repl/st
                  (resolve-symbols (get target 'novelty) target)
                  identity)]
         (:value res))))))

(defn build-target
  "Build a target, return true if work was done"
  [machfile target+verbs]

  (let [[target & verbs] (str/split target+verbs ":")
        target (resolve-target machfile target)]

    (if verbs
      (some identity (doall (for [verb verbs]
                              (apply-verb machfile target (symbol verb)))))
      (apply-verb machfile target nil))))

(defn mach [input]
  (let [targets (or (drop 5 (map symbol (.-argv nodejs/process))) ['default])]
    (let [machfile (reader/read-string input)
          machfile (postwalk (resolve-refs machfile) machfile)]
      (try
        (binding [cljs/*eval-fn* repl/caching-node-eval]
          (when-not
              (some identity
                    (doall (for [target targets]
                             (build-target machfile target))))
            (println "Nothing to do!")))

        (catch :default e
          (if-let [message (.-message e)]
            (println message)
            (println "Error:" e)))))))

;; Main
(mach (fs.readFileSync "Machfile.edn" "utf-8"))
