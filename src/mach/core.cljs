;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:refer-clojure :exclude [import])
  (:require
   [cljs.nodejs :as nodejs]
   [cljs.pprint :as pprint]
   [cljs.reader :as reader]
   [cljs.js :as cljs]
   [lumo.repl :as repl]
   [lumo.classpath]
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]))

(defonce ^:private st (cljs/empty-state))

;; Sorting

(def toposort (nodejs/require "toposort"))
(def path (nodejs/require "path"))
(def temp (nodejs/require "tmp"))

(defn target-order [machfile target-name]
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
        _ (apply println "$" args)
        temp (temp.tmpNameSync)
        result (.spawnSync child_process
                           (first args)
                           (clj->js (concat (map (comp #(str/replace % "'" "\\'")
                                                       #(str/replace % "|" "\\|")) (rest args))
                                            ["|" "tee" temp]))
                           #js {"shell" true
                                "stdio" "inherit"})]

    ;; TODO Handle stderror
    (.trim (str (fs.readFileSync temp)))))

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
  (lumo.repl/execute "file" cljs-file true true nil 0)
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
  (fn [machfile [target-name target] verb] verb))

(defmethod apply-verb :default [_ _ verb]
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

(defn- eval-rule
  "Evals Mach rule and returns the result if successful, throws an
  error if not."
  [code target machfile]
  (let [code (-> code
                 (resolve-symbols target)
                 (with-prop-bindings machfile))]
    ;; Eval the code
    (let [{:keys [value error]} (cljs/eval repl/st code identity)]
      (when error
        (throw (js/Error. (str "Could not eval form " code ", got error: " error))))
      value)))

(defn- spit-product [v target]
  (when-let [product (and (get target 'produce) (get target 'product))]
    (println "Writing" product)
    (fs.writeFileSync product v)))

(defn update! [machfile target & {:keys [post-op]
                                  :or {post-op spit-product}}]
  (let [code (if (map? target) (some target ['produce 'update!]) target)]
    (-> code
        (eval-rule target machfile)
        (post-op target))
    ;; We did work so return true
    true))

(defmethod apply-verb nil [machfile [target-name target] verb]
  (if-let [novelty-form (and (map? target) (get target 'novelty))]
    (let [novelty (eval-rule novelty-form target machfile)]
      ;; Call update!
      (when (or (true? novelty)
                (and (seq? novelty) (not-empty novelty)))
        (update! machfile (assoc target 'novelty `(quote ~novelty)))))

    ;; Target is an expr or there is no novelty, press on:
    (update! machfile target)))

;; Run the update (or produce) and print, no deps
(defmethod apply-verb 'update [machfile [target-name target] verg]
  (update! machfile target))

;; Print the produce
(defmethod apply-verb 'print [machfile [target-name target] verb]
  (update! machfile target :post-op (fn [v _] (println v))))

(defmethod apply-verb 'clean [machfile [target-name target] verb]
  (if-let [rule (get target 'clean!)]
    ;; If so, call it
    (eval-rule rule target machfile)
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
          :otherwise false))))
  true)

(defmethod apply-verb 'depends [machfile [target-name target] verb]
  (pprint/pprint
   (target-order machfile target-name))
  true)

(defmethod apply-verb 'novelty [machfile [target-name target] verb]
  (pprint/pprint
   (when-let [novelty (get target 'novelty)]
     (eval-rule novelty target machfile))))

(defn resolve-target
  "Resolve target key (symbol) matching given target (string) in machfile.
   Once a target has been resolved, it is also validated."
  [machfile target-name]
  (if-let [target-symbol (or (and (contains? machfile (symbol target-name)) (symbol target-name))
                             ;; Else try to search for product
                             (some (fn [[k v]]
                                     (when (= target-name (get v 'product))
                                       k))
                                   machfile))]
    (let [target (get machfile target-symbol)]
      ;; validate target contract:
      (when (and (get target 'produce)
                 (get target 'update!))
        (throw (ex-info "Invalid to have both update! and produce in the same target" {:target target})))
      ;; Validate dependency tree:
      (doseq [dep-target (rest (target-order machfile target-symbol))]
        (when-not (get machfile dep-target)
          (throw (ex-info (str "Target dependency not found: " dep-target) {}))))
      target-symbol)
    (throw (ex-info (str "Could not resolve target: " target-name) {}))))

(defn execute-plan [machfile build-plan]
  (into {} (for [[target-symbol verb] build-plan]
             [[target-symbol verb]
              (apply-verb machfile [target-symbol (get machfile target-symbol)] verb)])))

(defn build-plan [machfile [target-symbol verb]]
  (for [dependency-target (case verb
                            nil
                            (reverse (target-order machfile target-symbol))

                            'clean
                            (target-order machfile target-symbol)

                            [target-symbol])]
    [dependency-target verb]))

(defn- expand-out-target-and-verbs [machfile target+verbs]
  (let [[target-name & verbs] (str/split target+verbs ":")
        target-symbol (resolve-target machfile target-name)]
    (for [verb (if verbs (map symbol verbs) [nil])]
      [target-symbol verb])))

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
      (lumo.classpath/add! (clojure.string/split (str (fs.readFileSync cp-file)) ":"))))
  machfile)

(defn- preprocess-classpath [machfile]
  (when-let [cp (get machfile 'mach/classpath)]
    (js/$$LUMO_GLOBALS.addSourcePaths cp)))

(defn- preprocess-props [machfile]
  (if-let [props (get machfile 'mach/props)]
    (let [syms (map first (partition 2 props))
          vals (:value (cljs/eval repl/st `(let ~props
                                             [~@syms])
                                  identity))]
      (assoc machfile 'mach/props (vec (mapcat vector syms vals))))
    machfile))

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
                      (lumo.repl/execute "text" (str x) true false nil 0)
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
                                     preprocess-props
                                     preprocess-import
                                     preprocess-init
                                     preprocess-resolve-refs]))

(defn mach [input]
  (let [[opts args] (split-opts-and-args {} (rest (drop-while #(not= "mach/core.cljs" %) (.-argv nodejs/process))))
        machfile (-> opts
                     (get :f "Machfile.edn")
                     (fs.readFileSync "utf-8")
                     reader/read-string
                     preprocess)]
    (try
      (binding [cljs/*eval-fn* repl/caching-node-eval]
        (when-not (->> (or (seq (map symbol args)) ['default])
                       (mapcat (partial expand-out-target-and-verbs machfile))
                       (reduce (fn [m target-verb]
                                 (if (contains? m target-verb)
                                   (println (str "mach: '" (if-let [verb (second target-verb)]
                                                             (str (first target-verb) ":" verb) (first target-verb))
                                                 "' is up to date."))
                                   (let [build-plan (build-plan machfile target-verb)]
                                     (merge m (execute-plan machfile build-plan))))) {})
                       (vals)
                       (some identity))
          (println "Nothing to do!")))

      (catch :default e
        (if-let [message (.-message e)]
          (println message)
          (println "Error:" e))))))

;; Main
(mach [])
