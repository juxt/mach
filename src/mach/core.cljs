;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:require
   [cljs.nodejs :as nodejs]
   [cljs.pprint :as pprint]
   [cljs.reader :as reader]
   [cljs.js :as cljs]
   [lumo.repl :as repl]
   [aero.core :as aero]
   aero.aws ; aero aws credentials reader
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]))

(defonce ^:private st (cljs/empty-state))

;; Sorting

(def toposort (nodejs/require "toposort"))

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
                 [nil target]
                 )))))))

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
  (apply println "$" args)
  (let [result (.spawnSync child_process
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

(defn resolve-refs [machfile]
  (fn [x]
    (if (instance? Reference x)
      (get-in machfile (:path x))
      x)))

(defmulti apply-verb
  "Return boolean to indicate if work was done (true) or not (false)"
  (fn [machfile target verb] verb))

(defmethod apply-verb :default [machfile target verb]
  (throw (ex-info (str "Unknown verb: '" verb "'") {})))

(defn update! [recipe novelty verb]
  (when (and (get recipe 'produce)
             (get recipe 'update!))
    (throw (ex-info "Invalid to have both update! and produce in the same recipe" {:recipe recipe})))

  (let [code (resolve-symbols
              ;; Expression
              (if (map? recipe)
                (or (get recipe 'produce)
                    (get recipe 'update!)
                    )
                recipe)
              ;; Scope
              (if (map? recipe)
                (merge
                 recipe
                 ;; Already computed novelty
                 {'novelty `(quote ~novelty)})
                ;; Just a expression, no scope
                {}))]

    (when-let [val (:value (cljs/eval repl/st code identity))]
      ;; Print regardless
      (cond
        (= verb 'print) (println val)
        :otherwise (when (get recipe 'produce)
                     (when-let [product (get recipe 'product)]
                       (spit product val)))))

    ;; We did work so return true
    true))

(defmethod apply-verb nil [machfile target verb]
  (some identity
        (doall
         (for [target (reverse (order machfile target))
               :let [recipe (get machfile target)]]
           (do
             (if recipe
               (let [novelty (when (get recipe 'novelty)
                               (let [res (cljs/eval
                                          repl/st
                                          (resolve-symbols (get recipe 'novelty) recipe)
                                          identity)]
                                 (:value res)))]

                 ;; Call update!
                 (if (or (not (map? recipe))
                         (and (get recipe 'update!) (nil? (get recipe 'novelty)))
                         (true? novelty)
                         (when (seq? novelty) (not-empty novelty)))

                   (update! recipe novelty verb)))

               (throw (ex-info (str "No target: " target) {}))))))))

;; Run the update (or produce) and print, no deps
(defmethod apply-verb 'update [machfile target verb]
  (let [recipe (get machfile target)]
    (if recipe
      (update! recipe nil verb)
      (throw (ex-info (str "No target: " target) {})))))

;; Print the produce
(defmethod apply-verb 'print [machfile target verb]
  (let [recipe (get machfile target)]
    (if recipe
      (update! recipe nil verb)
      (throw (ex-info (str "No target: " target) {})))))

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

(defmethod apply-verb 'depends [machfile target verb]
  (pprint/pprint
   (order machfile target))
  true)

(defmethod apply-verb 'novelty [machfile target verb]
  (pprint/pprint
   (when-let [recipe (get machfile target)]
     (when (get recipe 'novelty)
       (let [res (cljs/eval
                  repl/st
                  (resolve-symbols (get recipe 'novelty) recipe)
                  identity)]
         (:value res))))))

(defn build-target
  "Build a target, return true if work was done"
  [machfile target:verb]
  (let [tv (str target:verb)
        ;; TODO: Cope with multiverbs
        ix (.indexOf tv ":")]
    (if (pos? ix)
      (let [target (symbol (subs tv 0 ix))
            verb (symbol (subs tv (inc ix)))]
        (apply-verb machfile target verb))
      (apply-verb machfile target:verb nil))))

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

;; Misc



(defmulti pre-process-json
  "Pre-process Clojure before JSON conversion according to a given
  style"
  (fn [v style]
    (case style
      :terraform :convert-dash-to-underscore)))

(defmethod pre-process-json :default [v style]
  v)

(defmethod pre-process-json :convert-dash-to-underscore [v style]
  (postwalk
   (fn [x]
     (if (and (vector? x) (= (count x) 2))
       (let [[k v] x]
         [(str/replace (name k) "-" "_") v])
       x))
   v))

(defn json
  ([v]
   (json v :terraform))
  ([v style]
   (js/JSON.stringify (clj->js (pre-process-json v style)) nil 4)))

;; Main
(mach (fs.readFileSync "Machfile.edn" "utf-8"))
