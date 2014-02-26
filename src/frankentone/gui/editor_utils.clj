(ns frankentone.gui.editor-utils
  (:use [seesaw core chooser mig keymap color]
        [clojure.java.io :only [file resource]])
  (:require [overtone.music time rhythm pitch]
            [seesaw.rsyntax :as rsyntax]
            [seesaw.keystroke :as keystroke])
  (:import
   (java.io Writer)
   (javax.swing.text DefaultEditorKit)
   (java.awt Container)
   (java.io File)
   (java.awt.im InputContext)
   (org.fife.ui.rtextarea ChangeableHighlightPainter
                          RTextAreaEditorKit$DecreaseFontSizeAction
                          RTextAreaEditorKit$IncreaseFontSizeAction
                          RTextScrollPane
                          RTextAreaEditorKit)
   (org.fife.ui.rsyntaxtextarea RSyntaxTextArea
                                RSyntaxTextAreaEditorKit$ToggleCommentAction
                                RSyntaxTextAreaHighlighter
                                RSyntaxTextAreaEditorKit
                                RSyntaxTextAreaDefaultInputMap
                                RSyntaxUtilities
                                RSyntaxDocument
                                TokenTypes)
   ;; (org.fife.ui.rsyntaxtextarea.folding LispFoldParser
   ;;                                      Fold)
   ))


(defn pimp-editor-keymap [^RSyntaxTextArea editor]
  (let [neo (and (= (.getCountry (.getLocale (InputContext/getInstance))) "US")
                 (= (subs (.getVariant (.getLocale (InputContext/getInstance)))
                          0 12)
                    "UserDefined_"))
        shortcuts (list
                   ["menu D" "menu SEMICOLON" "none"]
                   ["menu D" "menu D" "none"]
                   ["menu ENTER" "menu ENTER" "none"]
                   ["shift ENTER" "shift ENTER" "none"]
                   ["alt D" "alt SEMICOLON"
                    DefaultEditorKit/deleteNextWordAction]
                   ["alt DELETE" "alt DELETE"
                    RTextAreaEditorKit/rtaDeletePrevWordAction]
                   ["control A" "control D"
                    DefaultEditorKit/beginLineAction]
                   ["control E" "control F"
                    DefaultEditorKit/endLineAction]
                   ["control P" "control V"
                    DefaultEditorKit/upAction]
                   ["control N" "control J"
                    DefaultEditorKit/downAction]
                   ["control F" "control O"
                    DefaultEditorKit/forwardAction]
                   ["control B" "control N"
                    DefaultEditorKit/backwardAction]
                   ["control D" "control SEMICOLON"
                    DefaultEditorKit/deleteNextCharAction]
                   ["control K" "control Y"
                    RTextAreaEditorKit/rtaDeleteRestOfLineAction]
                   ["shift TAB" "shift TAB"
                    RSyntaxTextAreaEditorKit/rstaDecreaseIndentAction])]
    (let [input-map (.getInputMap editor)]
      (if neo
        (do (doall (map #(.remove input-map (keystroke/keystroke (first %)))
                        shortcuts))
            (doall (map #(.put input-map (keystroke/keystroke (second %))
                               (last %))
                        shortcuts)))
        (doall (map #(.put input-map (keystroke/keystroke (first %))
                           (last %))
                    shortcuts))))))


(declare get-context)


(defn make-context-highlighter [^RSyntaxTextArea editor-tab]
  (let [
        highlighter (.getHighlighter editor-tab)
        painter (ChangeableHighlightPainter. (color "#aaddff" 128))
        red-painter (ChangeableHighlightPainter. (color "#ffaaaa" 128))
        hl (atom nil)]
    (seesaw.invoke/signaller
     [e]
     (when @hl
       (.removeHighlight highlighter @hl))
     (when-let [new-hl (get-context editor-tab)]
       (reset! hl
               (.addHighlight highlighter
                              (first new-hl)
                              (second new-hl)
                              (if (last new-hl)
                                painter
                                red-painter)))))))


(defn make-editor-tab
  [^File file]
  (let [editor-tab (rsyntax/text-area
                    :text file
                    :syntax :clojure
                    :tab-size 2)]
    (pimp-editor-keymap editor-tab)
    ;;(.setCodeFoldingEnabled editor-tab true)
    (listen editor-tab
            #{:caret-update}
            (make-context-highlighter editor-tab))
    {:title (.getName file)
     :tip (.getPath file)
     :content  (RTextScrollPane. editor-tab)}))


(defmacro with-out-str-and-value
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [v# ~@body]
         (vector (str s#)
                 v#)))))


(defn get-line-boundaries [^RSyntaxTextArea editor pos]
  (let [line (.getLineOfOffset editor pos)
        start (.getLineStartOffset editor line)
        end (.getLineEndOffset editor line)]
    (list start end)))


(defn flash-region [^RSyntaxTextArea editor [start end]] 
  (let [highlighter (.getHighlighter editor)
        painter (ChangeableHighlightPainter. (color "#ffdd66" 128))
        hl (.addHighlight highlighter
                          start end
                          painter)]
    (Thread/sleep 128)
    (.setPaint painter (color "#ffddbb" 128))
    (Thread/sleep 128)
    (.setPaint painter (color "#ffdddd" 128))
    (.removeHighlight highlighter hl)))


(defn valid-token? [tok]
  (not (or (not tok)
           (not (.type tok))
           (= (.type tok) TokenTypes/NULL))))


(defn get-token-at-caret
  ([^RSyntaxTextArea editor]
     (get-token-at-caret editor 0))
  ([^RSyntaxTextArea editor offset]
     (let [position (max 0 (min (count (text editor))
                                (+ (config editor :caret-position) offset)))
           token (RSyntaxUtilities/getTokenAtOffset
                  (.getTokenListForLine
                   editor
                   (.getLineOfOffset
                    editor
                    position
                    ))
                  position)]
       token)))


(defn- closing-brak? [x] (or (= x \))
                             (= x \})
                             (= x \])))
(defn- opening-brak? [x] (or (= x \()
                             (= x \[)
                             (= x \{)))
(defn- get-opposite [x] (case x
                          \( \)
                          \{ \}
                          \[ \]
                          \) \(
                          \} \{
                          \] \[))

(defn get-context 
  "Returns a list of the offsets of the bracket pair the caret is in
  and true if the brackets match. Returns nil if no matching brackets
  were found.

  Limitations: Doesn't ignore comments."
  ([^RSyntaxTextArea editor]
     (get-context editor (config editor :caret-position)))
  ([^RSyntaxTextArea editor caret-position]
     (get-context editor caret-position caret-position))
  ([^RSyntaxTextArea editor start end]
     (let [
           document ^RSyntaxDocument (.getDocument editor)
           len-text (count (text editor))
           position (let [pos (max 0 (dec start))]
                      (if (closing-brak? (.charAt document pos))
                        pos
                        (inc pos)))    ;(max 0 (dec))
           opening-brak
           (loop [pos (max 0 (dec start))
                  lvl (list)]
             (when
                 (>= pos 0)
               (let [to-test (.charAt document pos)]
                 (cond
                  (and (seq lvl)
                       (= ^char (first lvl) to-test))
                  (recur (dec pos)
                         (rest lvl))

                  (closing-brak? to-test)
                  (recur (dec pos)
                         (conj lvl (get-opposite to-test)))
                  
                  (and (opening-brak? to-test)
                       (empty? lvl))
                  (list pos to-test)

                  :default
                  (recur (dec pos) lvl)))))]
       
       (when opening-brak 
         (let [closing (get-opposite (second opening-brak))
               closing-brak
               (loop [pos end
                      lvl (list)]
                 (when
                     (< pos len-text)
                   (let [to-test (.charAt document pos)]
                     (cond
                      (and (seq lvl)
                           (= ^char (first lvl) to-test))
                      (recur (inc pos)
                             (rest lvl))
                      
                      (opening-brak? to-test)
                      (recur (inc pos)
                             (conj lvl (get-opposite to-test)))
                      
                      (and (closing-brak? to-test)
                           (empty? lvl))
                      (list pos to-test (= ^char closing to-test))
                      
                      :default
                      (recur (inc pos) lvl)))))]
           
           (when closing-brak
             [
              (first opening-brak)
              (min len-text (inc (first closing-brak)))
              (last closing-brak)]))))))


(comment
  ;; This version isn't reliable enough as the FoldManager seems to
  ;; return strange folds if not fully parsed (?)
 (defn get-region-boundaries [^RSyntaxTextArea editor pos]
   ;; as the fold manager doesn't reparse on every keystroke, reparse
   ;; the file every time to ensure correct boundaries
   (.reparse (.getFoldManager editor))
   (when-let [region (.getDeepestFoldContaining
                      (.getFoldManager editor)
                      pos)]
     ;;(println region)
     (let [region (loop [fold region]
                    (if (.getParent fold)
                      (recur (.getParent fold))
                      fold))]
       (list (.getStartOffset ^Fold region)
             (min (inc (.getEndOffset ^Fold region)) (count (text editor))))))))


(defn get-matching-context
  "Returns a list of the offsets of the matching bracket pair the
  caret is in. Returns nil if no matching brackets were found.

  Limitations: Doesn't ignore comments."
  ([^RSyntaxTextArea editor pos]
     (get-matching-context editor pos pos))
  ([^RSyntaxTextArea editor start end]
     (let [context (get-context editor start end)]
       (when (and context
                  (last context))
         (butlast context)))))


(defn get-region-boundaries
  "Returns the region boundaries for the given caret position.

  Limitations: Doesn't ignore comments and brackets therein."
  [^RSyntaxTextArea editor pos]
  (when-let [region (get-matching-context editor pos)]
    (let [region (loop [fold region]
                   (let [next-fold (get-matching-context editor (dec (first fold)) (last fold))]
                     (if (and next-fold
                              (not= next-fold fold))
                       (recur next-fold)
                       fold)))]
      region)))


(defn factory-function?
  "Tests if a given symbol represents a positional factory function as
  created automatically by deftype and defrecord.

  (factory-function? #'frankentone.ugens/->tSinOsc)"
  [name]
  (let [to-compare "Positional factory function for class"
       len (count to-compare)]
   (when-let [doc (-> name
                      meta
                      :doc)]
     (and (> (count doc) len)
          (= (subs doc 0 len) to-compare)))))

