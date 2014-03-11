(ns frankentone.examples.entropy.bottles
  (:use [frankentone.entropy entropy]))

(defntropy bottles-of-beer []
  (dotimes [i 99]
    (let [amount (- 99 i)]
      (if (< amount 99)
        (println (str amount " bottles of beer on the wall.")))
      (println (str amount " bottles of beer on the wall, "
                    amount " bottles of beer."))
      (println (str "Take one down, pass it around, ")))))

;; run it
(bottles-of-beer)

;; selfmodificating version:
(defntropy-sm bottles-of-beer-sm []
  (dotimes [i 99]
    (let [amount (- 99 i)]
      (if (< amount 99)
        (println (str amount " bottles of beer on the wall.")))
      (println (str amount " bottles of beer on the wall, "
                    amount " bottles of beer."))
      (println (str "Take one down, pass it around, ")))))


(bottles-of-beer-sm)

