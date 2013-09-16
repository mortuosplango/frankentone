(ns frankentone.entropy.selfmod
  (:use [frankentone.entropy entropy]
        [frankentone utils])
  (:import
   (org.fife.ui.rsyntaxtextarea RSyntaxTextArea)))

(defn swap-val [editor start end pos value]
  (let [code-str (atom (subs
                        (seesaw.core/text editor)
                        start
                        end))
        code (read-string @code-str)
        position (atom 0)]
    (clojure.walk/postwalk
     (fn [input]
       (if (= (swap! position inc) pos)
         (swap! code-str
                clojure.string/replace-first
                (str input)
                (if (= (.indexOf (str input) "?") -1)
                  (str value)
                  (clojure.string/re-quote-replacement (str "?" value))))
         input))
     (nth code 3))
    @code-str))


(defn selfmod-cb [editor name pos value]
  (seesaw.core/invoke-later
   (let [target (.indexOf (seesaw.core/text editor)
                          name)
         old-text (seesaw.core/text editor)]
     (when (not= target -1)
       (when-let [bounds (if-let [region
                                  (frankentone.gui.editor/get-region-boundaries
                                   editor target)]
                           region
                           (frankentone.gui.editor/get-line-boundaries
                            editor target))]
         (let [old-caret (.getCaretPosition editor)
               [start end] bounds
               to-replace (swap-val editor start end pos value)
               new-text  (str (subs old-text 0 start)
                              to-replace
                              (subs old-text end))]
           (seesaw.core/text!
            editor
            new-text)
           (if (> old-caret end)
             (.setCaretPosition editor (min (count new-text)
                                            (+ (- old-caret
                                                  (- end start))
                                               (count to-replace))))
             (.setCaretPosition editor (min (count new-text)
                                            old-caret)))))))))


(defn make-selfmod []
  (partial selfmod-cb (frankentone.gui.editor/get-active-editor-tab)))

