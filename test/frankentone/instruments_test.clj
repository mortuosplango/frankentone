(ns frankentone.instruments-test
  (:use clojure.test
        frankentone.instruments))

(deftest definst-test
  (is (= (definst inst-test 0) nil))
  (is (= (definst inst-test (fn [x] 0.0)) nil))
  (is (= (definst inst-test (fn [x y z a b] 0.0)) nil))
  (is (= (definst inst-test (fn [x y z a] 'ahoi)) nil))
  (is (not (contains? @instruments :inst-test)))
  (is (not= (definst inst-test (fn [x y z a] 0.0)) nil))
  (is (contains? @instruments :inst-test)))
