;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:refer-clojure :exclude [import])
  (:require
   [clojure.set :refer [rename-keys]]
   [cljs.nodejs :as nodejs]
   [cljs.pprint :as pprint]
   [cljs.reader :as reader]
   [cljs.js :as cljs]
   [lumo.repl :as repl]
   [lumo.classpath]
   [clojure.walk :refer [postwalk]]
   [clojure.set :refer [map-invert]]
   [clojure.string :as str]))

(defonce ^:private st (cljs/empty-state))

;; Sorting

(def toposort (nodejs/require "toposort"))
(def path (nodejs/require "path"))
(def temp (nodejs/require "tmp"))
(def yargs (nodejs/require "yargs"))
(def mm (nodejs/require "micromatch"))

(defrecord Glob [glob])
(reader/register-tag-parser! "mach/glob" (fn [glob] (->Glob glob)))

(defprotocol Target
  (match? [this machfile]))

(extend-protocol Target
  cljs.core/Symbol
  (match? [this target-name]
    (= this (symbol target-name)))
  Glob
  (match? [this target-name]
    (mm.isMatch (str target-name) (:glob this))))

(defn resolve-target
  "Retrieve target-name from machfile as appropriate target with context added"
  [machfile target-name]
  (when-let [[matcher target]
             (or
               ;; TODO: Make is smarter about this, e.g. it prefers to
               ;; foo.min.js over foo.js when there is a target for both *.js
               ;; and *.min.js
               ;; We are currently non-deterministic in that case
               (some (fn [[k v]]
                       (when (match? k (str target-name))
                         [k v]))
                     machfile)
               ;; Else try to search for product
               ;; TODO: Does this make sense anymore now globs are available?
               ;; strings can be added for static "products" also
               (some (fn [[k v]]
                       (when (= target-name (get v 'product ::sentinel))
                         [k v]))
                     machfile))]
    (assoc target
           :mach/_target-ctx target-name
           :mach/_matcher-ctx matcher)))

(defn target-order [machfile target]
  (let [deps (tree-seq
               (fn [[_ target]]
                 (and (map? target)
                      (contains? target 'depends)))
               (fn [[_ target]]
                 (map (fn [target dependency]
                        [target (resolve-target machfile dependency)])
                      (repeat target)
                      (get target 'depends)))
               [nil target])
        ;; We want to use clojure's equality semantics with js, so we must turn
        ;; them to something that js can do equality on
        lookup (zipmap (into #{} cat deps)
                       (repeatedly (comp str gensym)))
        reverse-lookup (map-invert lookup)]
    (->> deps
         (map (fn [[k v]] [(lookup k) (lookup v)]))
         (clj->js)
         toposort
         (map reverse-lookup)
         rest)))

;; References

(defrecord Reference [path])

(defn ^:private read-reference [path]
  (->Reference path))

(reader/register-tag-parser! "ref" read-reference)

(def fs (nodejs/require "fs"))
(def child_process (nodejs/require "child_process"))
(def constants (nodejs/require "constants"))

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
  (lumo.classpath/add! [cp-file])
  `[])

(reader/register-tag-parser! "cp"  add-classpath-path-to-sources)

(defn add-classpath-path-file-to-sources [cp-file]
  (lumo.classpath/add! (clojure.string/split (str (fs.readFileSync cp-file)) ":"))
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
  (fn [machfile target verb] verb))

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

(def ^:dynamic *target-ctx* nil)
(def ^:dynamic *matcher-ctx* nil)

(defn- with-target-ctx-bindings
  [expr target]
  (list 'binding (into [] cat (-> target
                                  (select-keys [:mach/_target-ctx :mach/_matcher-ctx])
                                  (rename-keys {:mach/_target-ctx 'mach.core/*target-ctx*
                                                :mach/_matcher-ctx 'mach.core/*matcher-ctx*})))
        expr))

(defn- eval-rule
  "Evals Mach rule and returns the result if successful, throws an
  error if not."
  [code target machfile]
  (let [code (-> code
                 (resolve-symbols target)
                 (with-prop-bindings machfile)
                 (with-target-ctx-bindings target))]
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

(defmethod apply-verb nil [machfile target verb]
  (if-let [novelty-form (and (map? target) (get target 'novelty))]
    (let [novelty (eval-rule novelty-form target machfile)]
      ;; Call update!
      (when (or (true? novelty)
                (and (seq? novelty) (not-empty novelty)))
        (update! machfile (assoc target 'novelty `(quote ~novelty)))))

    ;; Target is an expr or there is no novelty, press on:
    (update! machfile target)))

;; Run the update (or produce) and print, no deps
(defmethod apply-verb 'update [machfile target verg]
  (update! machfile target))

;; Print the produce
(defmethod apply-verb 'print [machfile target verb]
  (update! machfile target :post-op (fn [v _] (println v))))

(defmethod apply-verb 'clean [machfile target verb]
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

(defmethod apply-verb 'depends [machfile target verb]
  (pprint/pprint
    (map :mach/_matcher-ctx (target-order machfile target)))
  true)

(defmethod apply-verb 'novelty [machfile target verb]
  (pprint/pprint
   (when-let [novelty (get target 'novelty)]
     (eval-rule novelty target machfile))))

(defn resolve-validate-target
  "Resolve and validate a target from a machfile"
  [machfile target-name]
  (if-let [target (resolve-target machfile target-name)]
    (do
      ;; validate target contract:
      (when (and (get target 'produce)
                 (get target 'update!))
        (throw (ex-info "Invalid to have both update! and produce in the same target" {:target target})))
      ;; Validate dependency tree:
      (doseq [dep-target (rest (target-order machfile target))]
        (when-not (resolve-target machfile dep-target)
          (throw (ex-info (str "Target dependency not found: " dep-target) {}))))
      target)
    (throw (ex-info (str "Could not resolve target: " target-name) {}))))

(defn execute-plan [machfile build-plan]
  (into {} (for [[target verb] build-plan]
             [[target verb]
              (apply-verb machfile target verb)])))

(defn build-plan [machfile [target verb]]
  (for [dependency-target (case verb
                            nil
                            (reverse (target-order machfile target))

                            'clean
                            (target-order machfile target)

                            [target])]
    [dependency-target verb]))

(defn- expand-out-target-and-verbs [machfile target+verbs]
  (let [[target-name & verbs] (str/split target+verbs ":")
        target (resolve-validate-target machfile target-name)]
    (for [verb (if verbs (map symbol verbs) [nil])]
      [target verb])))

(defn- preprocess-init [machfile]
  (when-let [target (get machfile 'mach/init)]
    (cljs/eval repl/st target identity))
  machfile)

(defn- preprocess-import [machfile]
  (when-let [imports (get machfile 'mach/import)]
    (into machfile
          (for [[extension props] imports
                [ext-k ext-target] (load-extension extension)]
            [ext-k (map-props-onto-extension-target ext-target props)]))))

(defn- write-classpath [cp-file cp-hash-file deps]
  (println "Writing Mach classpath to" cp-file)
  (let [result (.spawnSync child_process
                           "boot"
                           (clj->js (concat (mapv (fn [[sym v]] (str "-d " sym ":" (or v "RELEASE"))) deps)
                                            ["with-cp" "--write" "--file" cp-file]))
                           #js {"shell" true})]
    (if-not (= 0 (.-status result))
      (do (println (.toString (.-stderr result) "utf8"))
          (throw (js/Error. (str "Error while resolving dependencies"))))
      (fs.writeFileSync cp-hash-file
                        (hash deps)
                        #js {"flag" (bit-or constants.O_CREAT constants.O_SYNC constants.O_RDWR)}))))

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
    (lumo.classpath/add! cp)))

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

(defn mach [{:keys [file tasks constant]
             :or {file "Machfile.edn"
                  tasks '[default]
                  constant {}}}]
  (let [machfile (-> file
                     (fs.readFileSync "utf-8")
                     reader/read-string
                     (update 'mach/constant merge constant)
                     preprocess)]
    (try
      (binding [cljs/*eval-fn* repl/caching-node-eval]
        (when-not (->> tasks
                       (mapcat (partial expand-out-target-and-verbs machfile))
                       (reduce (fn [m [target verb :as target-verb]]
                                 (if (contains? m target-verb)
                                   (println (str "mach: '" (str (:mach/_matcher-ctx target-verb)
                                                                (when verb (str ":" verb)))
                                                 "' is up to date."))
                                   (let [build-plan (build-plan machfile target-verb)]
                                     (merge m (execute-plan machfile build-plan)))))
                               {})
                       (vals)
                       (some identity))
          (println "Nothing to do!")))

      (catch :default e
        (if-let [message (.-message e)]
          (println message)
          (println "Error:" e))))))

(defn- dissoc-nil
  "Remove keys if they are nil"
  ([m k]
   (if (nil? (get m k))
     (dissoc m k)
     m))
  ([m k & ks]
   (let [ret (dissoc-nil m k)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn -main
  [& args]
  (mach (-> yargs
            (.option "file" #js {"alias" "f"
                                 "describe" "Specify location of Machfile"
                                 "requiresArg" true
                                 "string" true})
            (.option "constant" #js {"describe" "Override the mach/constants section of your Machfile"
                                     "requiresArg" true
                                     "string" true
                                     "coerce" (fn [constant]
                                                (reader/read-string constant))})
            (.example "-f ~/myfile.edn" "Specify myfile.edn as location for Machfile")
            (.example "--constant '{environment :prod}'" "Override envrionment key in Machfile")
            (.epilog "Copyright © 2016-2017, JUXT LTD.")
            (.help)
            (.parse (clj->js (sequence args)))
            (js->clj :keywordize-keys true)
            ;; yargs adds the keys as "nil" when you use .option, but :or works better if you don't even have the key
            (dissoc-nil :file :f :constant)
            (rename-keys {:_ :tasks})
            (update :tasks #(when (seq %) (map symbol %))))))
