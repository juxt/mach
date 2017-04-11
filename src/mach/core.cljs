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

(defn order [machfile target-name]
  (map symbol
       (drop 1 ; drop nil
             (js->clj
              (toposort
               (clj->js
                (tree-seq
                 (fn [[_ target-name]] (-> machfile (get target-name) (get 'depends)))
                 (fn [[_ target-name]]
                   (map vector (repeat target-name) (-> machfile (get target-name) (get 'depends))))
                 [nil target-name])))))))

;; References

(defrecord Reference [path])

(defn ^:private read-reference [path]
  (->Reference path))

(reader/register-tag-parser! "ref" read-reference)

(def fs (nodejs/require "fs"))
(def child_process (nodejs/require "child_process"))

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

(defn ensure-mach-dir-exists []
  (when-not (fs.existsSync ".mach")
    (fs.mkdirSync ".mach" 0744)))

(defn ensure-mach-extensions-dir-exists []
  (ensure-mach-dir-exists)
  (when-not (fs.existsSync ".mach/extensions")
    (fs.mkdirSync ".mach/extensions" 0744)))

(defn modified-since [anchor source]
  (filter
   (partial modified-since? (apply max (conj (map last-modified (filter file? (file-seq anchor))) 0)))
   (filter (comp not mach.core/dir?) (if (coll? source)
                                       (mapcat mach.core/file-seq source)
                                       (mach.core/file-seq source)))))

(defn sh [& args]
  (let [args (flatten args)
        _  (apply println "$" args)
        result (.spawnSync child_process
                           (first args)
                           (clj->js (map (comp #(str/replace % "'" "\\'")
                                               #(str/replace % "|" "\\|")) (rest args)))
                           #js {"shell" true})]
    (println (str (.-stdout result)))
    (println (str (.-stderr result)))
    (when (.-stdout result)
      (.trim (str (.-stdout result))))))

(defn ^:private read-shell [vals]
  `(sh ~@vals))

(reader/register-tag-parser! "$" read-shell)

(defn ^:private read-shell-apply [vals]
  `(when (not-empty ~vals) (apply sh ~@vals)))

(reader/register-tag-parser! "$$" read-shell-apply)

(defn add-classpath-path-to-sources [cp-file]
  ;; TODO make work in Windows use a diff separator
  (js/$$LUMO_GLOBALS.addSourcePaths [cp-file])
  `[])

(reader/register-tag-parser! "cp"  add-classpath-path-to-sources)

(defn add-classpath-path-file-to-sources [cp-file]
   (js/$$LUMO_GLOBALS.addSourcePaths (clojure.string/split (str (fs.readFileSync cp-file)) ":"))
  `[])

(reader/register-tag-parser! "cpfile"  add-classpath-path-file-to-sources)

(defn ^:private eval-cljs [cljs-file]
  (lumo.repl/execute "file" cljs-file true true nil)
  `[])

(reader/register-tag-parser! "eval" eval-cljs)

(def ^:private extensions-cache (atom {}))

(declare preprocess)

(defn find-extension-file
  "Look for the extensions file. If it's not found in the current directory we look recursively through parent directories."
  [filename dirs]
  (when (> (count dirs) 1)
    (or (let [f (clojure.string/join path.sep (concat dirs [filename]))]
          (and (fs.existsSync f) f))
        (find-extension-file filename (drop-last dirs)))))

(defn ^:private read-extension-file [extension]
  (when-let [extensions-file (find-extension-file (str extension ".mach.edn")
                                                  (clojure.string/split (path.resolve ".") path.sep))]
    (reader/read-string (fs.readFileSync extensions-file "utf-8"))))

(defn ^:private fetch-extension-file [extension]
  (ensure-mach-extensions-dir-exists)

  (let [extensions-file (str ".mach/extensions/" (hash extension))]
    (when-not (fs.existsSync extensions-file)
      (let [result (.spawnSync child_process
                               "curl"
                               (clj->js ["-o" extensions-file extension])
                               #js {"shell" true})]
        (when-not (and (= 0 (.-status result)) (fs.existsSync extensions-file))
              (println (str (.-stdout result)))
              (println (str (.-stderr result)))

          (throw (js/Error. (str "Could not fetch extension " extensions-file))))))

    (reader/read-string (fs.readFileSync extensions-file "utf-8"))))

(defn ^:private add-extension [extensions extension]
  (assoc extensions extension (preprocess (cond (and (string? extension) (re-find #"^https?://.*" extension))
                                                (fetch-extension-file extension)

                                                :default
                                                (read-extension-file extension)))))

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

(defmulti apply-verb
  "Return boolean to indicate if work was done (true) or not (false)"
  (fn [machfile target-name verb] verb))

(defmethod apply-verb :default [machfile target-name verb]
  (throw (ex-info (str "Unknown verb: '" verb "'") {})))

(defn- with-prop-bindings [code machfile]
  (if-let [props (not-empty (get machfile 'mach/props))]
    `(let ~props
       ~code)
    code))

(defn- resolve-symbols
  "Postwalk and swap out symbols in the expression from supplied map."
  [expr target]
  (postwalk (fn [x] (or (and (symbol? x) (get target x)) x)) expr))

(defn- spit-product [target v]
  (when-let [product (and (get target 'produce) (get target 'product))]
    (println "Writing" product)
    (fs.writeFileSync product v)))

(defn update! [machfile target verb & {:keys [post-op]
                                       :or {post-op spit-product}}]
  (let [code (-> (if (map? target)
                   (resolve-symbols (some target ['produce 'update!]) target)
                   target)
                 (with-prop-bindings machfile))]

    ;; Eval the code
    (let [{:keys [value error]} (cljs/eval repl/st code identity)]
      (when error
        (throw (js/Error. (str "Could not eval form " code ", got error: " error))))

      (post-op target value))

    ;; We did work so return true
    true))

(defmethod apply-verb nil [machfile target-name verb]
  (some identity
        (doall
         (for [target-name (reverse (order machfile target-name))
               :let [target (get machfile target-name)]]
           (if (and (map? target) (get target 'novelty))
             (let [novelty (let [res (cljs/eval
                                      repl/st
                                      (resolve-symbols (get target 'novelty) target)
                                      identity)]
                             (:value res))]

               ;; Call update!
               (when (or (true? novelty)
                         (and (seq? novelty) (not-empty novelty)))
                 (update! machfile (assoc target 'novelty `(quote ~novelty)) verb)))

             ;; Target is an expr or there is no novelty, press on:
             (update! machfile target verb))))))

;; Run the update (or produce) and print, no deps
(defmethod apply-verb 'update [machfile target-name verb]
  (update! machfile (get machfile target-name) verb))

;; Print the produce
(defmethod apply-verb 'print [machfile target-name verb]
  (update! machfile (get machfile target-name) verb :post-op (fn [_ v] (println v))))

(defmethod apply-verb 'clean [machfile target-name verb]
  (doseq [target (order machfile target-name)
          :let [target (get machfile target-name)]]
    (if-let [rule (get target 'clean!)]
      ;; If so, call it
      (cljs/eval repl/st (resolve-symbols rule target) identity)
      ;; Otherwise implied policy is to delete declared target files
      (when-let [product (get target 'product)]
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
   (let [target (get machfile target-name)
         novelty (get target 'novelty)]
     (when novelty
       (let [res (cljs/eval
                  repl/st
                  (resolve-symbols novelty target)
                  identity)]
         (:value res))))))

(defn resolve-target
  "Resolve target key (symbol) matching given target (string) in machfile.
   Once a target has been resolved, it is also validated."
  [machfile target-name]
  (if-let [target (or (and (contains? machfile (symbol target-name)) (symbol target-name))
                      ;; Else try to search for product
                      (some (fn [[k v]]
                              (when (= target-name (get v 'product))
                                k))
                            machfile))]

    (do
      ;; validate target contract:
      (when (and (get target 'produce)
                 (get target 'update!))
        (throw (ex-info "Invalid to have both update! and produce in the same target" {:target target})))

      ;; Validate dependency tree:
      (doseq [target-name (reverse (order machfile target-name))]
        (when-not (get machfile target-name)
          (throw (ex-info (str "Target dependency not found: " target-name) {}))))
      target)

    (throw (ex-info (str "Could not resolve target: " target-name) {}))))

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

(defn- preprocess-init [machfile]
  (when-let [target (get machfile 'mach/init)]
    (cljs/eval repl/st target identity))
  machfile)

(defn- preprocess-import [machfile]
  (into machfile
        (for [[extension props] (get machfile 'mach/import)
              [ext-k ext-target] (load-extension extension)]
          [ext-k (map-props-onto-extension-target ext-target props)])))

(defn- write-classpath [cp-file cp-hash-file deps]
  (println "Writing Mach classpath to" cp-file)
  (let [result (.spawnSync child_process
                           "boot"
                           (clj->js (concat (mapv (fn [[sym v]] (str "-d " sym ":" (or v "RELEASE"))) deps)
                                            ["with-cp" "--write" "--file" cp-file]))
                           #js {"shell" true})]
    (when-not (and (= 0 (.-status result)) (fs.existsSync cp-file))
      (throw (js/Error. (str "Could not write classpath to " cp-file))))

    (fs.writeFileSync cp-hash-file (hash deps))))

(defn- preprocess-dependencies [machfile]
  (when-let [deps (or (get machfile 'mach/dependencies)
                      ;; Deprecated
                      (get machfile 'mach/m2))]
    (ensure-mach-dir-exists)
    (let [cp-file ".mach/cp"
          cp-hash-file ".mach/cp-hash"]
      (when-not (and (fs.existsSync cp-hash-file)
                     (= (hash deps) (reader/read-string (fs.readFileSync cp-hash-file "utf-8"))))
        (write-classpath cp-file cp-hash-file deps))
      (js/$$LUMO_GLOBALS.addSourcePaths (clojure.string/split (str (fs.readFileSync cp-file)) ":"))))
  machfile)

(defn- preprocess-classpath [machfile]
  (when-let [cp (get machfile 'mach/classpath)]
    (js/$$LUMO_GLOBALS.addSourcePaths cp)))

(defn- preprocess-requires
  "Ensure that the classpath has everything it needs, prior to targets being evaled"
  [machfile]
  ;; Process mach requires
  (when-let [requires (get machfile 'mach/require)]
    (doseq [req requires]
      (cljs/eval repl/st `(require '~req) identity)))
  (postwalk (fn [x]
              (cond (and (list? x) (= 'require (first x)))
                    (do
                      (lumo.repl/execute "text" (str x) true false nil)
                      nil)

                    ;; Auto require
                    (and (list? x) (symbol? (first x)) (namespace (first x)))
                    (let [ns (symbol (namespace (first x)))]
                      (when-not (find-ns ns)
                        (cljs/eval repl/st `(require '~ns) identity))
                      x)

                    :else x))
            machfile))

(defn- preprocess-resolve-refs [mach-config]
  (postwalk (fn [x]
              (if (instance? Reference x)
                (get-in mach-config (:path x))
                x))
            mach-config))

(defn preprocess [machfile]
  (reduce #(or (%2 %1) %1) machfile [preprocess-dependencies
                                     preprocess-classpath
                                     preprocess-requires
                                     preprocess-import
                                     preprocess-init
                                     preprocess-resolve-refs]))

(defn mach [input]
  (let [[opts args] (split-opts-and-args {} (drop 5 (.-argv nodejs/process)))
        targets (or (seq (map symbol args)) ['default])
        machfile (get opts :f "Machfile.edn")]
    (let [mach-config (reader/read-string (fs.readFileSync machfile "utf-8"))
          mach-config (preprocess mach-config)]
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
