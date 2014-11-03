;; based on entropy
;; https://github.com/rottytooth/Entropy/

(ns frankentone.entropy.entropy
  (:use clojure.walk))


(def mutation-rate
  "Maximum amount of change applied each time an Entropy data type is
  accessed."
  (atom 2.0))


(defn inverse-uniform-mutation
  "Given a number x, returns a number modified by a random amount
  inverse uniformly distributed between -mutation-rate and
  mutation-rate."
  [x]
  (let [mutate-amount (inc (rand-int 254))
        is-negative? (rand-nth [true false])
        change-amount (if is-negative?
                        (- (/ @mutation-rate
                              (double mutate-amount)))
                        (/ @mutation-rate
                           (double mutate-amount)))]
    (+ x change-amount)))


(def mutation-fn
  "Defines the function to run for every mutation"
  (atom inverse-uniform-mutation))


(defn make-real
  "Returns an entropy floating point number."
  ([initial-value]
     (let [value (atom (double initial-value))]
       (fn []
         (swap! value @mutation-fn))))
  ([initial-value callback-fn]
     (let [value (make-real initial-value)]
       (fn []
         (let [new-value (value)]
           (callback-fn new-value)
           new-value)))))

(defn make-char
  "Returns an entropy char."
  ([initial-value]
     (let [value (make-real (int initial-value))]
       (fn [] (unchecked-char (Math/round (value))))))
  ([initial-value callback-fn]
     (let [value (make-char initial-value)]
       (fn []
         (let [new-value (value)]
           (callback-fn new-value)
           new-value)))))

(defn make-string
  "Returns an entropy string."
  ([initial-value]
     (let [values (map #(make-char %) initial-value)]
       (fn [] (apply str (map #(%) values)))))
  ([initial-value callback-fn]
     (let [value (make-string initial-value)]
       (fn []
         (let [new-value (value)]
           (callback-fn new-value)
           new-value)))))


(defmacro fn->fntropy
  "Transforms every number, character or string in body into an entropy
  datatype."
  [name args body marked? callback-fn]
  (let [vars# (atom [])
        position# (atom 0)
        new-body# (postwalk
                   (fn [input#]
                     (swap! position# inc)
                     (if marked?
                       (if
                           (and (symbol? input#)
                                (not= (.indexOf (str input#) "?") -1))
                         (let [symstr# (subs (str input#)
                                             (inc (.indexOf (str input#) "?")))]
                           (if-let [number# (and (empty? (filter
                                                          #(= (.indexOf "0123456789.-e"
                                                                        (str %)) -1)
                                                          symstr#))
                                                 (read-string symstr#))]
                             (let [sym# (gensym "constant_")]
                               (swap! vars#
                                      conj sym#
                                      (if callback-fn
                                        `(make-real ~number#
                                                    (partial  ~callback-fn
                                                              ~(str name)
                                                              ~(deref position#)))
                                        `(make-real ~number#)))
                               (conj nil sym#))
                             input#))
                         input#)
                       ;; unmarked:
                       (if
                           (or (number? input#)
                               (char? input#)
                               (string? input#))
                         (let [sym# (gensym "constant_")]
                           (swap! vars#
                                  conj sym#
                                  (if callback-fn
                                    `(~(cond (number? input#) `make-real
                                             (char? input#) `make-char
                                             (string? input#) `make-string)
                                      ~input#
                                      (partial (eval ~callback-fn)
                                               ~(str name)
                                               ~(deref position#)))
                                    `(~(cond (number? input#) `make-real
                                             (char? input#) `make-char
                                             (string? input#) `make-string)
                                      ~input#)))
                           (conj nil sym#))
                         input#)))
                   body)]
    `(let ~(deref vars#)
       (fn ~args
         ~new-body#))))


(defmacro fntropy
  "Transforms every number, character or string in body into an entropy
  datatype."
  [args body]
  `(fn->fntropy nil ~args ~body false nil))

(defmacro defntropy
  "Transforms every number, character or string in body into an entropy
  datatype."
  [name args body]
  `(def ~name (fntropy ~args ~body)))


(defmacro fntropy-cb
  "Transforms every number, character or string in body into an entropy
  datatype.

  callback-fn is passed to every entropy datatype and called when its
  value changes. It has to be a function with 3 arguments: name of the
  function, position in code tree of the value, current value."
  [args body callback-fn]
  `(fn->fntropy nil ~args ~body false ~callback-fn))

(defmacro defntropy-cb
  "Transforms every number, character or string in body into an entropy
  datatype.

  callback-fn is passed to every entropy datatype and called when its
  value changes. It has to be a function with 3 arguments: name of the
  function, position in code tree of the value, current value."
  [name args body callback-fn]
  `(def ~name (fn->fntropy ~name ~args ~body false ~callback-fn)))


(defmacro fnt
  "Transforms every integer or floating point number prefixed with a ?
  in the function body into an entropy datatype."
  [args body]
  `(fn->fntropy nil ~args ~body true nil))

(defmacro defnt
  "Transforms every integer or floating point number prefixed with a ?
  in the function body into an entropy datatype."
  [name args body]
  `(def ~name (fnt ~args ~body)))


(defmacro fnt-cb
  "Transforms every integer or floating point number prefixed with a ?
  in the function body into an entropy datatype.

  callback-fn is passed to every entropy datatype and called when its
  value changes. It has to be a function with 3 arguments: name of the
  function, position in code tree of the value, current value."
  [args body callback-fn]
  `(fn->fntropy nil ~args ~body true ~callback-fn))

(defmacro defnt-cb
  "Transforms every integer or floating point number prefixed with a ?
  in the function body into an entropy datatype.

  callback-fn is passed to every entropy datatype and called when its
  value changes. It has to be a function with 3 arguments: name of the
  function, position in code tree of the value, current value."
  [name args body callback-fn]
  `(def ~name (fn->fntropy ~name ~args ~body true ~callback-fn)))
