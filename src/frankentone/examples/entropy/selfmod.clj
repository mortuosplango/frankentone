(ns frankentone.examples.entropy.selfmod
  (:use
   [frankentone utils]
   [frankentone.entropy entropy]
   [frankentone.gui editor]))

(defnt-cb hello [in] 
  (+ ?12.7 in)
  (partial selfmod-cb (get-active-editor-tab)))

(defntropy-cb hi [in] 
  (str (+ 12.7 in) "hilldihodihi")
  (make-selfmod))

(hi 12)

(hello 12)


;; first start MARY 4.3 server

(definst speak
  (fn [freq amp dur]
    (let [
          spcc (mem-render-string (str freq))
          limit (double (dec (count spcc)))
          len (/ limit 16000.0)]
      (fn [x]
        (if (< x len)
          (* amp (aget ^doubles spcc (min limit (* x 16000.0))))
          0.0)))))


(reset-dsp!
 (let [samp (atom 0.0)]
   (fn ^Double [x chan]
     (if (zero? chan)
       (reset! samp (speak x))
       @samp))))


(start-dsp)

(play-note (+ 0.1 (nows)) :speak "test" 0.2 2.0)

(defntropy-cb make-txt77 []
[	"hello, how are you?" ||
]
  (make-selfmod))

(defn pat1277 [t]
  (play-pattern (make-txt77)
                2.0 0.1 :speak)
  (let [next-t (+ t 2000)]
    (apply-at next-t  #'pat1277 [next-t]))))


(defn pat1277 [t]
  (play-pattern (make-txt77)
                2.0 0.1 :speak)
  (let [next-t (+ t 2000)]
    (apply-at next-t  #'pat1277 [next-t]))))


(pat1277 (+ 1000 (now)))

(defn pat1277
[_])


(stop-dsp)
