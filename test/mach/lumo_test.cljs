(ns mach.lumo-test
  (:require [mach.core-test]
            [cljs.test :refer-macros [run-tests]]))

(defn -main [& argv]
  (run-tests 'mach.core-test))
