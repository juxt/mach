;; Copyright © 2016-2017, JUXT LTD.

(ns aero.core
  (:require
   [cljs.nodejs :as nodejs]
   [cljs.tools.reader :as r]
   [clojure.walk :refer [postwalk]]))

;; Aero support
;;
;; This is mostly just copy-and-paste from Aero code When
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

(defmethod reader 'case
  [opts tag [k choices]]
  (let [v (get opts k)]
    (cond (contains? choices v) (get choices v)
          (contains? choices :default) (get choices :default)
          :otherwise nil)))

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

(defmethod reader 'join
  [opts tag value]
  (apply str value))

(defmethod reader 'uri
  [opts tag value]
  value)
