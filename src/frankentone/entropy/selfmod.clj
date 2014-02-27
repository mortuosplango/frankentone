(ns frankentone.entropy.selfmod
  (:use [frankentone.entropy entropy]
        [frankentone utils])
  (:require [frankentone.gui editor editor-utils])
  (:import
   (org.fife.ui.rsyntaxtextarea RSyntaxTextArea)))


(defn- swap-in-code-str! [code-str input value marked?]
  (swap! code-str
         clojure.string/replace-first
         (if (nil? input)
           "nil"
           (print-str input))
         (if marked?
           (clojure.string/re-quote-replacement (print-str "?" value))
           (print-str value))))

(defn swap-val [^RSyntaxTextArea editor marked? start end pos value body-pos]
  (let [code-str (atom (subs
                        (seesaw.core/text editor)
                        start
                        end))
        position (atom 0)
        value (if (float? value) 
                (scround value 0.001)
                value)
        parsed-code (read-string @code-str)]
    (if (> (count parsed-code) body-pos)
      (if (zero? pos)
        (let [input (nth parsed-code body-pos)]
          (swap-in-code-str! code-str input value marked?))
        (do
          (clojure.walk/postwalk
           (fn [input]
             (if (= (swap! position inc) pos)
               (do
                 ;;(println "WALK" position pos input)
                 (swap-in-code-str! code-str input value marked?))
               input))
           ;; parse the string and
           ;; walk only the body of the function
           (nth parsed-code body-pos))
          @code-str))
      (println "ERROR: parsed code: " @code-str " body-pos: " body-pos))))


(defn selfmod-cb [^RSyntaxTextArea editor marked? name pos value 
                  & { :keys [body-pos]
                     :or {body-pos 3}}]
  (let [;; find target function by name
        target (.indexOf ^String (seesaw.core/text editor)
                         ^String name)
        ;; save the old text
        old-text (seesaw.core/text editor)]
    (when (not= target -1) ;; if a target is found
      ;; get the region of the target function in the string
      (when-let [[start end] (if-let [region
                                      (frankentone.gui.editor-utils/get-region-boundaries
                                       editor target)]
                               region
                               (frankentone.gui.editor-utils/get-line-boundaries
                                editor target))]
        (let [;; save old caret position and selection to restore it later
              old-caret (.getCaretPosition editor)
              old-selection (seesaw.core/selection editor)]
          ;; only change if caret isn't inside the function to be changed
          (when-not (and (>= old-caret start)
                         (< old-caret end))
            (let [
                  ;; do the swapping only on the region
                  to-replace (swap-val editor marked? start end pos value body-pos)]
              ;; replace the region in the text editor
              (.replaceRange editor
                             to-replace
                             start end)
              (let [new-pos (fn [x]
                              (min (count (seesaw.core/text editor))
                                   (if (> x end)
                                     (+ (- x
                                           (- end start))
                                        (count to-replace))
                                     x)))]
                ;; restore the caret position and selection
                (.setCaretPosition editor (new-pos old-caret))
                (when old-selection
                  (seesaw.core/selection! editor (map new-pos old-selection)))))))))))


(defn make-selfmod [marked? & { :keys [body-pos] :or {body-pos 3}}]
  (let [tab (frankentone.gui.editor/get-active-editor-tab)
        cb (partial selfmod-cb tab marked?)]
    (seesaw.invoke/signaller
     [name pos value]
     (cb name pos value :body-pos body-pos))))


(defmacro defntropy-sm
  "Transforms every number, character or string in body into an entropy
  datatype."
  [name args body]
  `(def ~name (fn->fntropy ~name ~args ~body false (make-selfmod false))))


(defmacro defnt-sm
  "Transforms every integer or floating point number prefixed with a ?
  in the function body into an entropy datatype."
  [name args body]
  `(def ~name (fn->fntropy ~name ~args ~body true (make-selfmod true))))

