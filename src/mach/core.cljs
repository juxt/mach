#!/usr/bin/lumo

;; Copyright © 2016-2017, JUXT LTD.

(ns mach.core
  (:require [cljs.nodejs :as nodejs]
            [cljs.reader :as reader]
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
   (filter (comp not mach.core/dir?) (mach.core/file-seq source))))

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

(defn step [machfile k message?]
  (if-let [v (get machfile k)]
    (do
      (doseq [dep (get v 'depends)]
        (step machfile dep false))
      (let [novelty (when (get v 'novelty)
                      (:value
                       (cljs/eval
                        repl/st
                        (resolve-keywords (get v 'novelty) v)
                        identity)))]

        ;; Call update!
        (if (or (nil? (get v 'novelty))
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
            (cljs/eval repl/st
                       code
                       identity))
          (when message? (println "Nothing to do!")))))
    (println "No target:" k)))

(defn mach [err input]
  (let [target (symbol (first (drop 3 (.-argv nodejs/process))))]
    (if err
      (println "ERROR")
      (let [machfile (reader/read-string input)
            machfile (postwalk (resolve-refs machfile) machfile)]

        (binding [cljs/*eval-fn* repl/caching-node-eval]
          (step machfile target true))))))

(.readFile fs "Machfile.edn" "utf-8" mach)

(defn act [err input]
  (if err
    (println "input error")
    (println "Read input ok:" (reader/read-string input))))

(defn config []
  (println "Testing")
  #_(.readFile fs "/home/malcolm/src/kermit/resources/config.edn" "utf-8" act)
  (.readFile fs "foo.edn" "utf-8" act)
  )

(def profile :prod)

(defn read-profile [value]
  (println "Reading" value)
  "hi")

(reader/register-tag-parser!
 "custom"
 read-profile)
