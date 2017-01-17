#!/usr/bin/lumo

;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:require
   [cljs.nodejs :as nodejs]
   [cljs.pprint :as pprint]
   [cljs.reader :as reader]
   [cljs.tools.reader :as r]
   [cljs.js :as cljs]
   [lumo.repl :as repl]
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

(defn exec-verb [machfile target verb]
  (cond
    (= verb 'tree)
    (do
      (pprint/pprint
       (order machfile target))
      true)

    (= verb 'clean)
    (do
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

    (= verb :default)
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
                     (let [code (resolve-symbols (if (map? recipe)
                                                   (get recipe 'update!)
                                                   recipe)
                                                 (if (map? recipe)
                                                   (merge
                                                    recipe
                                                    ;; Already computed novelty
                                                    {'novelty `(quote ~novelty)})
                                                   {}))]
                       (do
                         (cljs/eval repl/st code identity)
                         ;; We don't do this because we have spit instead
                         #_(binding [*print-fn*
                                   ;; Write to the product if it's declared
                                   (if-let [product (get recipe 'product)]
                                     (fn [x] (fs.writeFileSync product x))
                                     *print-fn*)]
                           (cljs/eval repl/st code identity))
                         true))))

                 (throw (ex-info (str "No target: " target) {})))))))

    :otherwise (throw (ex-info (str "Unknown verb: '" verb "'") {}))))

(defn build-target
  "Build a target, return true if work was done"
  [machfile target:verb]
  (let [tv (str target:verb)
        ix (.indexOf tv ":")]
    (if (pos? ix)
      (let [target (symbol (subs tv 0 ix))
            verb (symbol (subs tv (inc ix)))]
        (exec-verb machfile target verb))
      (exec-verb machfile target:verb :default))))

(defn mach [input]
  (let [targets (or (drop 3 (map symbol (.-argv nodejs/process))) ['default])]
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

;; Aero support This is mostly just copy-and-paste from Aero code When
;; I've worked out how to include other cljs namespaces into a
;; lumo-based product, or even reference aero as a dependency, then
;; I'm sure JUXT will produce a cljs version of Aero we can use
;; directly. Still 'early days' here.

(declare read-config)

(defmulti reader (fn [opts tag value] tag))

(defmethod reader :default
  [_ tag value]
  (throw (ex-info (str "No reader for tag: " tag) {:tag tag :value value})))

(defmethod reader 'env
  [opts tag value]
  (aget js/process.env value))

(defmethod reader 'profile
  [{:keys [profile]} tag value]
  (cond (contains? value profile) (get value profile)
        (contains? value :default) (get value :default)
        :otherwise nil))

(defmethod reader 'user
  [{:keys [user]} tag value]
  (let [user (or user js/process.env.USER)]
    (or
     (some (fn [[k v]]
             (when (or (= k user)
                       (and (set? k) (contains? k user)))
               v))
           value)
     (get value :default))))

(defmethod reader 'include
  [{:keys [resolver source] :as opts} tag value]
  (read-config
   (if (map? resolver)
     (get resolver value)
     (resolver source value))
   opts))

(defmethod reader 'join
  [opts tag value]
  (apply str value))

(defmethod reader 'aws-kms-decrypt
  [opts tag value]
  "XXX")

(defn- get-in-ref
  [config]
  (letfn [(get-in-conf [m]
            (postwalk
             (fn [v]
               (if-not (contains? (meta v) :ref)
                 v
                 (get-in-conf (get-in config v))))
             m))]
    (get-in-conf config)))

(def default-opts
  {:profile :default
   :resolver (fn [source include]
               (if (.startsWith include "/")
                 include
                 (str source include)))})

(defn read-config [source given-opts]
  (let [opts (merge default-opts given-opts {:source source})]
    (get-in-ref
     (binding [r/*default-data-reader-fn* (partial reader opts)]
       (r/read-string (fs.readFileSync source "utf-8"))))))

(defn spit [f data]
  (fs.writeFileSync f data))

;; Main
(mach (fs.readFileSync "Machfile.edn" "utf-8"))
