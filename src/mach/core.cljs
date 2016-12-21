#!/home/malcolm/bin/lumo

;; Copyright © 2016, JUXT LTD.

(ns mach.core
  (:require [cljs.nodejs :as nodejs]
            [cljs.reader :as reader]
            [cljs.js :as cljs]
            [lumo.repl :as repl]
            [clojure.walk :refer [postwalk]]))

(defonce ^:private st (cljs/empty-state))

;; References

(defrecord Reference [path])

(defn ^:private read-reference [path]
  (->Reference path))

(reader/register-tag-parser! "ref" read-reference)

(def fs (nodejs/require "fs"))

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

(defn file-seq [dir]
  (if (.existsSync fs dir)
    (tree-seq
     dir?
     (fn [d] (map (partial str d "/") (seq (.readdirSync fs d))))
     dir)
    []))

(defn modified-since [anchor source]
  (filter
   (partial modified-since? (apply max (conj (map last-modified (filter dir? (file-seq anchor))) 0)))
   (filter (comp not mach.core/dir?) (mach.core/file-seq source))))

(defn resolve-keywords [expr scope]
  (postwalk (fn [x]
              (if (symbol? x)
                (if-let [v (get scope x)] v x)
                x))
            expr))

(defn sh [& args]
  (apply println "$" args))

(defn ^:private read-shell [vals]
  `(sh ~@vals))

(reader/register-tag-parser! "$" read-shell)

(defn ^:private read-shell-apply [vals]
  `(apply sh ~@vals))

(reader/register-tag-parser! "$$" read-shell-apply)

(defn resolve-refs [makefile]
  (fn [x]
    (if (instance? Reference x)
      (get-in makefile (:path x))
      x)))

(defn step [makefile k]
  (let [v (get makefile k)
        _  (doseq [dep (get v 'mach/depends)]
             (step makefile dep))
        novelty
        (when (get v 'mach/novelty)
          (:value
           (cljs/eval
            repl/st
            (resolve-keywords (get v 'mach/novelty) v)
            identity)))]

    ;; Call update!
    (when (or (nil? (get v 'mach/novelty))
              (not-empty novelty))
      (let [code (resolve-keywords (if (map? v)
                                     (get v 'mach/update!)
                                     v)
                                   (if (map? v)
                                     (merge
                                      v
                                      ;; Already computed novelty
                                      {'mach/novelty `(quote ~novelty)})
                                     {}))]
        (cljs/eval repl/st
                   code
                   identity)))))

(defn make [err input]
  (let [target (symbol (first (drop 3 (.-argv nodejs/process))))]
    (if err
      (println "ERROR")
      (let [makefile (reader/read-string input)
            makefile (postwalk (resolve-refs makefile) makefile)]

        (binding [cljs/*eval-fn* repl/caching-node-eval]
          (step makefile target))))))

(.readFile fs "Makefile.edn" "utf-8" make)
