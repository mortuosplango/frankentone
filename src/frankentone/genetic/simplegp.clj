;; original author: Lee Spector (lspector@hampshire.edu) 20111018
;; based on https://gist.github.com/lspector/1384682/

(ns frankentone.genetic.simplegp
  (:use [frankentone utils
         ugens]
        [frankentone.genetic 
         analysis
         simplegpfunctions
         utils])
  (:require [incanter core charts stats])
  (:import [edu.emory.mathcs.jtransforms.fft FloatFFT_1D]))

(def ^:dynamic random-functions
  (list
   { :fn 'sin :arity 1 }
   { :fn 'cos :arity 1 }
   { :fn 'tanh :arity 1 }
   { :fn 'mul-sin :arity 2 }
   { :fn 'mul-cos :arity 2 }
   { :fn 'mul-tanh :arity 2 }
   ;;{ :fn 'rrand :arity 1 }
   ;;{ :fn 'rrand :arity 2 }
   { :fn '+ :arity 2 }
   { :fn '+ :arity 3 }
   { :fn '- :arity 2 }
   { :fn '* :arity 2 }
   { :fn '* :arity 3 }
   { :fn 'pmod :arity 2 }
   { :fn 'pd :arity 2 }
   { :fn 'pround :arity 2 }
   { :fn 'max :arity 2 }
   { :fn 'min :arity 2 }
   { :fn 'mean :arity 2 }
   { :fn 'if>0 :arity 3 }
   { :fn 'if<0 :arity 3 }))

(def ^:dynamic random-terminals
  '(list
    'x
    (rrand -5.0 5.0)
    'Math/PI
    (rrand -1.0 1.0)))

(defn random-function
  "Return a random function with its arity.

  E. g. {:fn 'functionname :arity 4}"
  []
  (rand-nth random-functions))

(defn random-function-with-arity
  [arity]
  (rand-nth (filter #(= (:arity %) arity) random-functions)))


(defn random-terminal
  "Return a random terminal."
  []
  (rand-nth (eval random-terminals)))


(defn random-code
  "Return random code of a given depth."
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [random-fn (random-function)]
      (conj
       (repeatedly
        (:arity random-fn)
        (fn [] (random-code (dec depth))))
       (:fn random-fn)))))


;; We can now generate and evaluate random small programs, as with:

;; (let [i (random-code 3)] (println (error i) "from individual" i))

;; To help write mutation and crossover functions we'll write a utility
;; function that injects something into an expression and another that
;; extracts something from an expression.

(defn codesize
  "Returns the code size in points."
  [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn inject
  "Returns a copy of individual i with new inserted randomly somewhere
  within it (replacing something else)."
  [new i]
  (if (seq? i)
    (if (zero? (rand-int (count (flatten i))))
      new
      (case (dec (count i))
        1 (list (first i) (inject new (nth i 1)))
        2 (rand-nth-weighted
           [[
             (list (first i) (inject new (nth i 1)) (nth i 2))
             (codesize (nth i 1))]
            [
             (list (first i) (nth i 1) (inject new (nth i 2)))
             (codesize (nth i 2))]])
        3 (rand-nth-weighted
           [[
             (list (first i) (inject new (nth i 1)) (nth i 2) (nth i 3))
             (codesize (nth i 1))]
            [
             (list (first i) (nth i 1) (inject new (nth i 2)) (nth i 3))
             (codesize (nth i 2))]
            [
             (list (first i) (nth i 1) (nth i 2) (inject new (nth i 3)))
             (codesize (nth i 3))]])))
    new))<

(defn vary
  "Returns a copy of individual i with a terminal varied or a function
  exchanged for another of the same arity.

  TODO: function/terminal variation should have the same possibility."
  [i]
  (if (seq? i)
    (if (zero? (rand-int (count (flatten i))))
      (conj (rest i)
            (:fn (random-function-with-arity (dec (count i)))))
      (case (dec (count i))
        1 (list (first i) (vary (nth i 1)))
        2 (rand-nth
           [
            (list (first i) (vary (nth i 1)) (nth i 2))
            (list (first i) (nth i 1) (vary (nth i 2)))])
        3 (rand-nth
           [
            (list (first i) (vary (nth i 1)) (nth i 2) (nth i 3))
            (list (first i) (nth i 1) (vary (nth i 2)) (nth i 3))
            (list (first i) (nth i 1) (nth i 2) (vary (nth i 3)))])))
    (if (number? i)
      (* i (rrand 0.5 1.5))
      (random-terminal))))>

(defn extract
  "Returns a random subexpression of individual i."
  [i]
  (if (seq? i)
    (if (zero? (rand-int (count (flatten i))))
      i
      (extract (nth i (rand-nth-weighted
                       (mapv
                        (fn [n elt]
                          [(inc n) (codesize elt)])
                        (range (dec (count i)))
                        (rest i))))))
    i))

;; Now the mutate and crossover functions are easy to write:

(defn mutate
  [i]
  (inject (random-code 2) i))

(defn crossover
  [i j]
  (inject (extract j) i))

;; We can see some mutations with:
;; (let [i (random-code 2)] (println (mutate i) "from individual" i))

;; and crossovers with:
;;(let [i (random-code 2) j (random-code 2)] (println (crossover i j) "from" i "and" j))

;; We'll also want a way to sort a population by error that doesn't require 
;; lots of error re-computation:

(defn sort-by-error
  "Sort a given population by error returned by error-fn."
  [population error-fn]
  (mapv second
        (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
              (map #(vector (error-fn %) %) population))))

;; Finally, we'll define a function to select an individual from a sorted 
;; population using tournaments of a given size.

(defn select
  "Select an individual from a sorted population using tournaments of a
  given size."
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

;; Now we can evolve a solution by starting with a random population and 
;; repeatedly sorting, checking for a solution, and producing a new 
;; population.

(def ^:dynamic *evolution* (atom true))


(defn evolve
  "Start evolution.

  Error-fn has to accept an individual and return an error factor (the
  higher, the worse).

  Optional best-callback will accept the best program and its error."
  [popsize error-fn & { :keys  [best-callback stop-atom success-threshold
                                mutation-rate crossover-rate clone-rate
                                vary-rate random-code-rate
                                functions terminals]
                       :or   {best-callback nil
                              stop-atom *evolution*
                              success-threshold 0.1
                              mutation-rate 0.35
                              crossover-rate 0.25
                              clone-rate 0.23
                              vary-rate 0.13
                              random-code-rate 0.04
                              functions random-functions
                              terminals random-terminals}}]
  (println "Starting evolution...")
  (reset! stop-atom true)
  (binding [random-functions functions
            random-terminals terminals]
    (loop [generation 0
           population (sort-by-error (repeatedly popsize #(random-code 2)) error-fn)]
      (let [best (first population)
            best-error (error-fn best)]
        (println "======================")
        (println "Generation:" generation)
        (println "Best error:" best-error)
        (println "Best program:" best)
        (when (fn? best-callback)
          (best-callback best best-error))
        (println "     Median error:" (error-fn (nth population 
                                                     (int (/ popsize 2)))))
        (println "     Average program size:" 
                 (float (/ (reduce + (map count (map flatten population)))
                           (count population))))
        
        ;; good enough to count as success
        (if (or (not @stop-atom) (< best-error success-threshold)) 
          (println "Success:" best)
          (recur 
           (inc generation)
           (time (sort-by-error      
                  (concat
                   (repeatedly (* mutation-rate popsize) #(mutate (select population 7)))
                   (repeatedly (* crossover-rate popsize) #(crossover (select population 7)
                                                                      (select population 7)))
                   (repeatedly (* clone-rate popsize) #(select population 7))
                   (repeatedly (* vary-rate popsize) #(vary (select population 7)))
                   (repeatedly (* random-code-rate popsize) #(random-code (inc (rand-int 20)))))
                  error-fn))))))))

