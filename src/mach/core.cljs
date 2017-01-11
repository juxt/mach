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

;; References

(defrecord Reference [path])

(defn ^:private read-reference [path]
  (->Reference path))

(reader/register-tag-parser! "ref" read-reference)

(def fs (nodejs/require "fs"))
(def child_process (nodejs/require "child_process"))

(defn file-exists? [f]
  (.existsSync fs f))

(defn last-modified [f]
  (if f
    (if (file-exists? f)
      (.getTime (.-mtime (.statSync fs f)))
      0)
    0))

(defn modified-since? [since f]
  (< since (last-modified f)))

(defn mkdir [path]
  (.mkdirSync fs path))

(defn dir? [f]
  (.. fs (lstatSync f) (isDirectory)))

(defn file? [f]
  (.. fs (lstatSync f) (isFile)))

(defn files
  ([dir]
   (when (dir? dir)
     (seq (.readdirSync fs dir))))
  ([]
   (files ".")))

(defn has-suffix [suffix]
  #(.endsWith % suffix))

(defn file-seq [dir]
  (if (.existsSync fs dir)
    (tree-seq
     dir?
     (fn [d] (map (partial str d "/") (seq (.readdirSync fs d))))
     dir)
    []))

(defn modified-since [anchor source]
  (filter
   (partial modified-since? (apply max (conj (map last-modified (filter file? (file-seq anchor))) 0)))
   (filter (comp not mach.core/dir?) (if (coll? source)
                                       (mapcat mach.core/file-seq source)
                                       (mach.core/file-seq source)))))

(defn resolve-keywords [expr scope]
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
                           #js {"shell" true})]))

(defn ^:private read-shell [vals]
  `(sh ~@vals))

(reader/register-tag-parser! "$" read-shell)

(defn ^:private read-shell-apply [vals]
  `(apply sh ~@vals))

(reader/register-tag-parser! "$$" read-shell-apply)

(defn resolve-refs [machfile]
  (fn [x]
    (if (instance? Reference x)
      (get-in machfile (:path x))
      x)))

(defn step [machfile k]
  (if-let [v (get machfile k)]
    (let [work-done (some identity (doall (for [dep (get v 'depends)]
                                            (step machfile dep))))
          novelty (when (get v 'novelty)
                    (let [res (cljs/eval
                               repl/st
                               (resolve-keywords (get v 'novelty) v)
                               identity)]
                      (:value res)))]

      ;; Call update!
      (if (or work-done
              (not (map? v))
              (and (get v 'update!) (nil? (get v 'novelty)))
              (true? novelty)
              (when (seq? novelty) (not-empty novelty)))
        (let [code (resolve-keywords (if (map? v)
                                       (get v 'update!)
                                       v)
                                     (if (map? v)
                                       (merge
                                        v
                                        ;; Already computed novelty
                                        {'novelty `(quote ~novelty)})
                                       {}))]
          (do (cljs/eval repl/st code identity)
              true))

        ))

    (println "No target:" k)))

(defn mach [err input]
  (let [targets (or (drop 3 (map symbol (.-argv nodejs/process))) ['default])]
    (if err
      (println "ERROR")
      (let [machfile (reader/read-string input)
            machfile (postwalk (resolve-refs machfile) machfile)]

        (binding [cljs/*eval-fn* repl/caching-node-eval]
          (when-not
              (some identity
                    (doall (for [target targets]
                             (step machfile target))))
            (println "Nothing to do!")))))))

(.readFile fs "Machfile.edn" "utf-8" mach)

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
       (r/read-string (.readFileSync fs source "utf-8"))))))

(defn spit [f data]
  (.writeFileSync fs f data))
