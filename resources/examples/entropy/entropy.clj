(ns frankentone.examples.entropy.entropy
  (:use [frankentone.entropy entropy]))

;; entropice chars, strings and numbers
(defntropy hello []
  (println "hello" \D 12))

(doall (repeatedly 10 hello))

;; entropice only numbers with ? prefix
(defnt hello []
  (println "hello" \D 12 ?12 ?13.0))

(doall (repeatedly 10 hello))

;; entropice with callback
(defnt-cb hello [in]
  (+ ?12.7 (* ?17 in))
  (fn [a b c] (println "callback"
                      "|| fn name: " a
                      "|| position in code: " b
                      "|| new value: " c)))

(doall (repeatedly 10 (partial hello 12)))

