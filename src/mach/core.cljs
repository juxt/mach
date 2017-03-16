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
(def path (nodejs/require "path"))

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
  (if (and f (file-exists? f))
    (.getTime (.-mtime (fs.statSync f)))
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

(defn add-classpath-path-to-sources [cp-file]
  `[~::cp ~cp-file])

(reader/register-tag-parser! "cp"  add-classpath-path-to-sources)

(defn add-classpath-path-file-to-sources [cp-file]
  `[~::cp-file ~cp-file])

(reader/register-tag-parser! "cpfile"  add-classpath-path-file-to-sources)

(defn ^:private eval-cljs [cljs-file]
  `[~::eval ~cljs-file])

(reader/register-tag-parser! "eval" eval-cljs)

(def ^:private extensions-cache (atom {}))

(defn find-extension-file
  "Look for the extensions file. If it's not found in the current directory we look recursively through parent directories."
  [filename dirs]
  (when (> (count dirs) 1)
    (or (let [f (clojure.string/join path.sep (concat dirs [filename]))]
          (and (fs.existsSync f) f))
        (find-extension-file filename (drop-last dirs)))))

(defn ^:private add-extension [extensions extension]
  (if-let [extensions-file (find-extension-file (str extension ".mach.edn")
                                                (clojure.string/split (path.resolve ".") path.sep))]
    (assoc extensions extension (reader/read-string (fs.readFileSync extensions-file "utf-8")))
    extensions))

(defn- load-extension [extension]
  (let [extensions (or (get @extensions-cache extension)
                       (get (swap! extensions-cache add-extension extension) extension))]
    (when-not extensions
      (throw (js/Error. (str "Could not find extensions file for extension " extension))))
    extensions))

(defn- map-props-onto-extension-target [target props]
  (postwalk (fn [v]
              (type v)
              (if (symbol? v)
                (get props v v) v))
            target))

(defn import [[target args]]
  (let [[extension k] (str/split (str target) "/")]
    (-> extension
        load-extension
        (get (symbol k))
        (map-props-onto-extension-target args))))

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

    ;; We do a post walk to ensure tha the classpath has everything it needs, prior to eval'ing
    (postwalk (fn [x]
                (cond (and (vector? x) (= ::eval (first x)))
                      (do
                        (lumo.repl/execute "file" (second x) true true nil)
                        nil)

                      (and (vector? x) (= ::cp (first x)))
                      (do
                        ;; TODO make work in Windows use a diff separator
                        (js/$$LUMO_GLOBALS.addSourcePaths [(second x)])
                        nil)

                      (and (vector? x) (= ::cp-file (first x)))
                      (do
                        ;; TODO make work in Windows use a diff separator
                        (js/$$LUMO_GLOBALS.addSourcePaths (clojure.string/split (str (fs.readFileSync (second x))) ":"))
                        nil)

                      (and (list? x) (= 'require (first x)))
                      (do
                        (lumo.repl/execute "text" (str x) true false nil)
                        nil)

                      ;; Auto require
                      (and (list? x) (symbol? (first x)) (namespace (first x)))
                      (let [ns (symbol (namespace (first x)))]
                        (when-not (find-ns ns)
                          (cljs/eval repl/st `(require '~ns) identity))
                        nil)

                      :else x))
              code)

    ;; Eval the code
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
               (throw (ex-info (str "Target not found: " target-name) {}))))))))

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

(defn- split-opts-and-args [opts args]
  (let [[k v & rest-args] args]
    (if (and k v (re-find #"^\-\w$" k))
      (split-opts-and-args (assoc opts (keyword (.substring k 1)) v)
                           rest-args)
      [opts args])))

(defmulti apply-mach-preprocess
  "Preprocess Machfile with available configurations"
  (fn [machfile verb target] verb))

(defmethod apply-mach-preprocess :import [machfile verb extensions]
  (for [[extension props] extensions
        [ext-k ext-target] (load-extension extension)]
    [ext-k (map-props-onto-extension-target ext-target props)]))

(defmethod apply-mach-preprocess :default [machfile verb target]
  (throw (ex-info (str "Unknown preprocess target: '" verb "'") {})))

(defn preprocess [machfile]
  (reduce into {}
        (for [[k v] machfile]
          (if-let [verb (keyword (last (re-find #"^mach/(\w+)" (str k))))]
            (apply-mach-preprocess machfile verb v)
            [[k v]]))))

(defn mach [input]
  (let [[opts args] (split-opts-and-args {} (drop 5 (.-argv nodejs/process)))
        targets (or (seq (map symbol args)) ['default])
        machfile (get opts :f "Machfile.edn")]
    (let [mach-config (reader/read-string (fs.readFileSync machfile "utf-8"))
          mach-config (preprocess (postwalk (resolve-refs mach-config) mach-config))]
      (try
        (binding [cljs/*eval-fn* repl/caching-node-eval]
          (when-not
              (some identity
                    (doall (for [target targets]
                             (build-target mach-config target))))
            (println "Nothing to do!")))

        (catch :default e
          (if-let [message (.-message e)]
            (println message)
            (println "Error:" e)))))))

;; Main
(mach [])
