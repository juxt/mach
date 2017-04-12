(ns mach.core-test
  (:require [cljs.test :refer [deftest is testing]]
            [mach.core :as mach]))

(deftest test-order-dedup-deps
  (is (= (list 'foo 'tar 'bar) (mach/order {'bar  nil
                                            'tar {'depends ['bar]
                                                  'update! nil}
                                            'foo {'depends ['tar 'bar]
                                                  'update! nil}}
                                           'foo))))
