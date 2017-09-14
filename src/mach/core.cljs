;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:refer-clojure :exclude [import])
  (:require
   [clojure.set :refer [rename-keys]]
   [cljs.nodejs :as nodejs]
   [cljs.pprint :as pprint]
   [cljs.reader :as reader]
   [cljs.js :as cljs]
   [lumo.repl]
   [lumo.classpath]
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defonce ^:private st (cljs/empty-state))

;; Sorting

(def toposort (nodejs/require "toposort"))
(def path (nodejs/require "path"))
(def temp (nodejs/require "tmp"))
(def yargs (nodejs/require "yargs"))
(def process (nodejs/require "process"))

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

(defn sh
  ([args]
   (sh args {}))
  ([args {:keys [quiet] :as opts}]
   ;; TODO check code and barf if non-zero?
   (go
     (let [args (flatten args)
           _ (apply println "$" args)
           b (atom (js/Buffer.alloc 0))
           cp (.spawn child_process
                      (first args)
                      (clj->js (rest args))
                      #js {"shell" true})
           c (a/chan)]
       (.on (.-stdout cp) "data"
            (fn [d]
              (when-not quiet
                (.write (.-stdout process) d))
              (swap! b #(js/Buffer.concat (clj->js [% d])))))
       (.on (.-stderr cp) "data" (fn [d] (.write (.-stderr process) d)))
       (.on cp "close" (fn [d] (go (a/>! c (.trim (.toString @b))))))
       (a/<! c)))))

(defn ^:private read-shell [vals]
  `(cljs.core.async/<! (sh ~vals)))

(reader/register-tag-parser! '$ read-shell)

(defn ^:private read-shell-apply [vals]
  `(when (not-empty ~vals)
     (cljs.core.async/<!
      (sh (concat (drop-last ~vals)
                  (last ~vals))))))

(reader/register-tag-parser! '$$ read-shell-apply)

(defn add-classpath-path-to-sources [cp-file]
  (lumo.classpath/add! [cp-file])
  `[])

(reader/register-tag-parser! 'cp  add-classpath-path-to-sources)

(defn add-classpath-path-file-to-sources [cp-file]
  (lumo.classpath/add! (clojure.string/split (str (fs.readFileSync cp-file)) ":"))
  `[])

(reader/register-tag-parser! 'cpfile  add-classpath-path-file-to-sources)

(defn ^:private eval-cljs [cljs-file]
  (lumo.repl/execute "file" cljs-file true true nil 0)
  `[])

(reader/register-tag-parser! 'eval eval-cljs)

(def ^:private extensions-cache (atom {}))

(def ^:private load-fn cljs.js/*load-fn*)

(defn code-eval [code]
  (binding [cljs/*eval-fn* lumo.repl/caching-node-eval
            cljs.js/*load-fn* load-fn]
    (let [{:keys [value error] :as res} (cljs/eval lumo.repl/st code identity)]
      (if error
        (throw (js/Error. (str "Could not eval form " code ", got error: " error)))
        value))))

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

(defn- load-extension-no-cache [extension]
  (go
    (let [extension-code (cond (and (string? extension) (re-find #"^https?://.*" extension))
                               (fetch-extension-file extension)

                               :default
                               (read-extension-file extension))]
      (a/<! (preprocess extension-code)))))

(defn- load-extension [extension]
  (go
    (or (get @extensions-cache extension)
      (let [loaded-extension (a/<! (load-extension-no-cache extension))
            extensions (swap! extensions-cache assoc extension loaded-extension)]
        (or (get extensions extension)
            (throw (js/Error. (str "Could not find extensions file for extension " extension))))))))

(defn- map-props-onto-extension-target [target props]
  (postwalk (fn [v]
              (if (symbol? v)
                (get props v v) v))
            target))

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

(defn- with-async-go [code]
  `(identity (cljs.core.async.macros/go
               ~code)))

(defn- eval-rule
  "Evals Mach rule and returns the result if successful, throws an
  error if not."
  [code target machfile]
  (-> code
      (resolve-symbols target)
      (with-prop-bindings machfile)
      (with-async-go)
      code-eval))

(defn- spit-product [v target]
  (when-let [product (and (get target 'produce) (get target 'product))]
    (println "Writing" product)
    (fs.writeFileSync product v)))

(defn update! [machfile target & {:keys [post-op]
                                  :or {post-op spit-product}}]
  (go
    (let [code (if (map? target) (some target ['produce 'update!]) target)
          v (a/<! (eval-rule code target machfile))]
      (post-op v target)
      ;; We did work so return true
      true)))

(defmethod apply-verb nil [machfile [target-name target] verb]
  (go
    (if-let [novelty-form (and (map? target) (get target 'novelty))]
      (let [novelty (a/<! (eval-rule novelty-form target machfile))]
        ;; Call update!
        (when (or (true? novelty)
                  (and (seq? novelty) (not-empty novelty)))
          (<! (update! machfile (assoc target 'novelty `(quote ~novelty))))))

      ;; Target is an expr or there is no novelty, press on:
      (<! (update! machfile target)))))

;; Run the update (or produce) and print, no deps
(defmethod apply-verb 'update [machfile [target-name target] verg]
  (go
    (<! (update! machfile target))))

;; Print the produce
(defmethod apply-verb 'print [machfile [target-name target] verb]
  (go
    (update! machfile target :post-op (fn [v _] (println v)))))

(defmethod apply-verb 'clean [machfile [target-name target] verb]
  (go
    (if-let [rule (get target 'clean!)]
      ;; If so, call it
      (<! (eval-rule rule target machfile))
      ;; Otherwise implied policy is to delete declared target files
      (when-let [product (get target 'product)]
        (if (coll? product)
          (sh (concat ["rm" (if (some dir? product) "-rf" "-f")] product))
          (cond
            (dir? product) (sh ["rm" "-rf" product])
            (file-exists? product) (sh ["rm" "-f" product])
            ;; er? this is overridden later
            :otherwise false))))
    true))

(defmethod apply-verb 'depends [machfile [target-name target] verb]
  (go
    (pprint/pprint
     (target-order machfile target-name))
    true))

(defmethod apply-verb 'novelty [machfile [target-name target] verb]
  (go
    (pprint/pprint
     (when-let [novelty (get target 'novelty)]
       (eval-rule novelty target machfile)))))

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

(defn build-plan [machfile [target-symbol verb]]
  (for [dependency-target (case verb
                            nil
                            (reverse (target-order machfile target-symbol))

                            'clean
                            (target-order machfile target-symbol)

                            [target-symbol])]
    [dependency-target verb]))

(defn execute-plan [machfile build-plan]
  (go-loop [results {} [target-and-verb & build-plan] build-plan]
    (if-let [[target-symbol verb] target-and-verb]
      (recur
       (assoc results [target-symbol verb]
              (a/<! (apply-verb machfile
                                [target-symbol (get machfile target-symbol)] verb)))
       build-plan)
      results)))

(defn execute-plan-async [machfile target-verbs]
  (let [execution-plans (a/chan)]
    (go
      ;; Make the plan
      (doseq [target-verb target-verbs]
        (a/>! execution-plans [target-verb (build-plan machfile target-verb)]))
      (a/close! execution-plans))

    ;; Execute the plan
    (go-loop [results {}]
      (when-let [[target-verb build-plan] (a/<! execution-plans)]
        (if (contains? results target-verb)
          (println (str "mach: '" (if-let [verb (second target-verb)]
                                    (str (first target-verb) ":" verb)
                                    (first target-verb))
                        "' is up to date."))
          (let [plan-result (a/<! (execute-plan machfile build-plan))]
            (recur (merge results plan-result)))))
      results)))

(defn- expand-out-target-and-verbs [machfile target+verbs]
  (let [[target-name & verbs] (str/split target+verbs ":")
        target-symbol (resolve-target machfile target-name)]
    (for [verb (if verbs (map symbol verbs) [nil])]
      [target-symbol verb])))

(defn- preprocess-init [machfile]
  (go
    (some-> machfile
            (get 'mach/init)
            (code-eval))
    machfile))

(defn- preprocess-import [machfile]
  (go-loop [machfile machfile [import & imports] (get machfile 'mach/import)]
    (if-let [[extension props & opts] import]
      (let [opts (apply hash-map opts)
            exts (into {}
                       (for [[k target] (a/<! (load-extension extension))
                             :let [k (cond (:as opts)
                                           (symbol (str (:as opts)) (str k))

                                           (:rename opts)
                                           (get (:rename opts) k k)

                                           :default
                                           k)]]
                         [k (map-props-onto-extension-target target props)]))]
        (recur (merge machfile exts) imports))
      machfile)))

(defn- write-classpath [cp-file cp-hash-file deps]
  (println "Writing Mach classpath to" cp-file)
  (let [result (.spawnSync child_process
                           "boot"
                           (clj->js (concat (mapv (fn [[sym v]] (str "-d " sym ":" (or v "RELEASE"))) deps)
                                            ["-B" "with-cp" "--write" "--file" cp-file]))
                           #js {"shell" true})]
    (if-not (= 0 (.-status result))
      (do (println (.toString (.-stderr result) "utf8"))
          (throw (js/Error. (str "Error while resolving dependencies"))))
      (fs.writeFileSync cp-hash-file
                        (hash deps)
                        #js {"flag" (bit-or constants.O_CREAT constants.O_SYNC constants.O_RDWR)}))))

(defn- preprocess-dependencies [machfile]
  (go
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
    machfile))

(defn- preprocess-npm [machfile]
  (go
    (doseq [[package version] (get machfile 'mach/npm)
            :when (not (= version (try (.-version (nodejs/require (str package "/package.json")))
                                       (catch :default e))))]
      (println "npm installing" package version)
      (.spawnSync child_process
                  "npm"
                  ["install" (str package "@" version)]
                  #js {"shell" true}))))

(defn- preprocess-classpath [machfile]
  (go
    (when-let [cp (get machfile 'mach/classpath)]
      (lumo.classpath/add! cp))))

(defn- preprocess-props [machfile]
  (go
    (if-let [props (get machfile 'mach/props)]
      (let [syms (map first (partition 2 props))
            code (-> `(let ~props
                        [~@syms])
                     (with-async-go))
            vals (a/<! (code-eval code))]
        (assoc machfile 'mach/props (vec (mapcat vector syms vals))))
      machfile)))

(defn- preprocess-requires
  "Ensure that the classpath has everything it needs, prior to targets being evaled"
  [machfile]
  (go
    (when-let [requires (get machfile 'mach/require)]
      (doseq [req requires]
        (code-eval `(require '~req))))
    (postwalk (fn [x]
                (cond (and (list? x) (= 'require (first x)))
                      (do
                        (lumo.repl/execute "text" (str x) true false nil 0)
                        nil)

                      ;; Auto require
                      (and (symbol? x) (namespace x) (not= "js" (namespace x)))
                      (let [ns (symbol (namespace x))]
                        (when-not (find-ns ns)
                          (code-eval `(require '~ns)))
                        x)

                      :else x))
              machfile)))

(defn- preprocess-resolve-refs [mach-config]
  (go
    (postwalk (fn [x]
                (if (instance? Reference x)
                  (get-in mach-config (:path x))
                  x))
              mach-config)))

(defn- preprocess-eval-product
  "product can be an expression, eval it."
  [mach-config]
  (go
    (into {}
          (for [[k m] mach-config]
            [k (if-let [product (and (map? m) (get m 'product))]
                 (let [code (with-prop-bindings `(identity ~product) mach-config)]
                   (assoc m 'product (code-eval code)))
                 m)]))))

(defn preprocess [machfile]
  (go-loop [machfile machfile
            [preprocessor & preprocessors] [preprocess-dependencies
                                            preprocess-npm
                                            preprocess-classpath
                                            preprocess-requires
                                            preprocess-props
                                            preprocess-import
                                            preprocess-init
                                            preprocess-resolve-refs
                                            preprocess-eval-product]]
    (if preprocessor
      (recur (or (a/<! (preprocessor machfile)) machfile) preprocessors)
      machfile)))

(defn mach [{:keys [file tasks constant]
             :or {file "Machfile.edn"
                  tasks '[default]
                  constant {}}}]

  (go
    (let [machfile (a/<! (-> file
                             (fs.readFileSync "utf-8")
                             reader/read-string
                             (update 'mach/constant merge constant)
                             preprocess))]

      ;; TODO review exception handling and test it
      (try
        (let [results (a/<! (->> tasks
                                 (mapcat (partial expand-out-target-and-verbs machfile))
                                 (execute-plan-async machfile)))]
          (when-not (->> results
                         (vals)
                         (some identity))
            (println "Nothing to do!")))
        (catch :default e
          (if-let [message (.-message e)]
            (println message)
            (println "Error:" e)))))))

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
            (.example "--constant '{environment :prod}'" "Override environment key in Machfile")
            (.epilog "Copyright © 2016-2017, JUXT LTD.")
            (.help)
            (.parse (clj->js (sequence args)))
            (js->clj :keywordize-keys true)
            ;; yargs adds the keys as "nil" when you use .option, but :or works better if you don't even have the key
            (dissoc-nil :file :f :constant)
            (rename-keys {:_ :tasks})
            (update :tasks #(when (seq %) (map symbol %))))))
