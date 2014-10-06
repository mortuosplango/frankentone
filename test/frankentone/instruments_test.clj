(ns frankentone.instruments-test
  (:use clojure.test
        frankentone.instruments))

(deftest definst-test
  (is (= (definst inst-test 0) nil))
  (is (= (definst inst-test (fn [x] 0.0)) nil))
  (is (= (definst inst-test (fn [x y z a b] 0.0)) nil))
  (is (= (definst inst-test (fn [x y z a] (fn [x] 'ahoi))) nil))
  (is (not (contains? @instruments :inst-test)))
  (is (not= (definst inst-test (fn [x y z & _] (fn [a] 0.0))) nil))
  (is (contains? @instruments :inst-test)))

(deftest play-note-test
  (is (do (definst inst-test (fn [x y z & _] (fn [a] 1.0)))
          (play-note 1.0 :inst-test 440.0 1.0 1.0)
          (= (inst-test 0.05) 0.0)))
  (is (= (inst-test 1.0) 1.0))
  (is (= (inst-test 1.5) 1.0))
  (is (= (inst-test 2.0) 0.0))
  (is (= (inst-test 20.0) 0.0)))

(deftest play-and-clear-test
  (is (do (definst inst-test (fn [x y z & _] (fn [a] 1.0)))
          (dotimes [i 20]
            (play-note (rand 1.0) :inst-test 440.0 1.0 1.0))
          (inst-test 1.0)
          (and (= (count @(.notes (:inst-test @instruments))) 20)
               (= (.peek (.note-starts (:inst-test @instruments))) nil))))
  (is (do (clear (:inst-test @instruments))
          (and (= @(.notes (:inst-test @instruments)) {})
               (= (.peek (.note-starts (:inst-test @instruments))) nil)))))
