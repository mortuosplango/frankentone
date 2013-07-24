;; based on the seesaw text-editor example:
;; https://github.com/daveray/seesaw/blob/develop/test/seesaw/test/examples/text_editor.clj

(ns frankentone.gui.editor
  (:use seesaw.core
        seesaw.chooser
        seesaw.mig
        [clojure.java.io :only [file resource]])
  (:require [seesaw.rsyntax :as rsyntax])
  (:require [seesaw.keystroke :as keystroke])
  (:require [frankentone.dsp :as dsp])
  (:require [frankentone.instruments])
  (:require [frankentone.patterns])
  (:require [frankentone.utils])
  (:require [frankentone.ugens])
  (:require [overtone.music.time])
  (:import [java.io Writer])
  (:import
   (javax.swing.text DefaultEditorKit)
   (java.awt Container)
   (org.fife.ui.rtextarea RTextAreaEditorKit)
   (org.fife.ui.rsyntaxtextarea RSyntaxTextAreaEditorKit)
   (org.fife.ui.rsyntaxtextarea RSyntaxUtilities)
   (org.fife.ui.rsyntaxtextarea RSyntaxTextAreaDefaultInputMap)
   (org.fife.ui.rsyntaxtextarea TokenTypes)
   (org.fife.ui.rtextarea RTextScrollPane)
   (org.fife.ui.rsyntaxtextarea.folding LispFoldParser)))


(native!)

(defn pimp-editor-keymap [editor]
  (let [input-map (.getInputMap editor)]
    (doto input-map
      (.put
       (keystroke/keystroke "menu D")
       "none")
      (.put
       (keystroke/keystroke "alt D")
       DefaultEditorKit/deleteNextWordAction)
      (.put
       (keystroke/keystroke "alt DELETE")
       RTextAreaEditorKit/rtaDeletePrevWordAction)
      (.put
       (keystroke/keystroke "control A")
       DefaultEditorKit/beginLineAction)
      (.put
       (keystroke/keystroke "control E")
       DefaultEditorKit/endLineAction)
      (.put
       (keystroke/keystroke "control P")
       DefaultEditorKit/upAction)
      (.put
       (keystroke/keystroke "control N")
       DefaultEditorKit/downAction)
      (.put
       (keystroke/keystroke "control F")
       DefaultEditorKit/forwardAction)
      (.put
       (keystroke/keystroke "control B")
       DefaultEditorKit/backwardAction)
      (.put
       (keystroke/keystroke "control D")
       DefaultEditorKit/deleteNextCharAction)
      (.put
       (keystroke/keystroke "control K")
       RTextAreaEditorKit/rtaDeleteRestOfLineAction))))


(def open-files (atom {
                       (.getPath (file (System/getProperty "user.home") ".ftscratch"))
                       (file (System/getProperty "user.home") ".ftscratch")}))

(when-not (.exists (val (first @open-files))) (spit (val (first @open-files)) ""))

(def current-file-label (label :text "" :font "SANSSERIF-PLAIN-8"))

(defn make-editor-tab [file]
  (let [editor-tab (rsyntax/text-area
                    :text file
                    :syntax :clojure
                    :tab-size 4)]
    (pimp-editor-keymap editor-tab)
    {:title (.getName file)
     :tip (.getPath file)
     :content  (RTextScrollPane. editor-tab)}))

(def editor
  (tabbed-panel
   :id :tabs
   :placement :top
   :tabs [ (make-editor-tab (val (first @open-files)))]))

(defn get-active-editor-tab []
  (.getComponent (.getComponent (:content (selection editor)) 0) 0))

(defn add-editor-tab-to-editor [editor-tab]
  (doto editor
    (.addTab (:title editor-tab)
             (make-widget (:content editor-tab)))
    (.setToolTipTextAt (dec (.getTabCount editor)) (:tip editor-tab))
    (.setSelectedIndex (dec (.getTabCount editor)))))

(defn get-current-file []
  (val (find  @open-files (.getToolTipTextAt editor (.getSelectedIndex editor)))))

;;(add-editor-tab-to-editor (make-editor-tab @current-file))

(def post-buffer
  (text :multi-line? true :font "MONOSPACED-PLAIN-10"
        :text ";;;;; Post buffer ;;;;;; \n \n \n"))


(def documentation-buffer
  (text :multi-line? true :font "MONOSPACED-PLAIN-12"
        :text "Welcome to frankentone!"))


(def split-view (left-right-split editor
                                  (top-bottom-split (scrollable post-buffer)
                                                    (scrollable documentation-buffer)
                                                    :divider-location 1/3)
                                  :divider-location 3/5))


(defn buffer-writer [buffer]
  (proxy [java.io.StringWriter] []
    (close [])
    (flush [])
    (write
      ([thing]
         (invoke-later
          (.append buffer thing)))
      ([thing start end]
         (invoke-later
          (.append buffer (subs (str thing) start end)))))))


(def status-label (label :text ""))


(defn set-status [& strings] (text! status-label (apply str strings)))


(def main-panel
  (mig-panel
   :constraints ["fill, ins 0"]
   :items [
           [split-view "grow"]
           [status-label "dock south"]
           [(separator) "dock south"]
           [current-file-label "dock south"]]))

(defn select-file [type] (choose-file main-panel :type type))


(defn a-new [e]
  (let [selected (select-file :save)]
    (if (.exists selected)
      (alert "File already exists.")
      (do
        (swap! open-files assoc (.getPath selected) selected)
        (spit selected "")
        (add-editor-tab-to-editor
         (make-editor-tab selected))
          ;;(text! (get-active-editor-tab) "")
        (set-status "Created a new file.")))))


(defn a-open [e]
  (let [selected (select-file :open)]
    (swap! open-files assoc (.getPath selected) selected)
    (add-editor-tab-to-editor
     (make-editor-tab selected))
    (set-status "Opened " selected ".")))


(defn a-save [e]
  (spit (get-current-file) (text (get-active-editor-tab)))
  (set-status "Wrote " (get-current-file) "."))


(defn a-save-as [e]
  (when-let [selected (select-file :save)]
    (swap! open-files dissoc (.getPath (get-current-file)))
    (swap! open-files assoc (.getPath selected) selected)
    (spit selected (text (get-active-editor-tab)))
    (.setTitleAt editor (.getSelectedIndex editor) (.getName selected))
    (.setToolTipTextAt editor (.getSelectedIndex editor) (.getPath selected))
    (set-status "Wrote " selected ".")))


(defn a-exit  [e] (dispose! e))
(defn a-copy  [e] (.copy (get-active-editor-tab)))
(defn a-cut   [e] (.cut (get-active-editor-tab)))
(defn a-paste [e] (.paste (get-active-editor-tab)))


(defn eval-string [to-eval]
  (let [result
        (try (binding [*out* (buffer-writer post-buffer)]
               (load-string
                (str "(ns frankentone.live
  (:use frankentone.dsp
        frankentone.ugens
        frankentone.utils
        frankentone.patterns
        frankentone.instruments
        overtone.music.time
        clojure.repl)) \n" 
                     to-eval)))
             (catch Exception e e))]
    (invoke-later (set-status "Result: " result)
                  (.append post-buffer (str result))
                  (.append post-buffer "\n")
                  (scroll! post-buffer :to :bottom))
    result))


(defn show-documentation [symbol]
  (text! documentation-buffer "")
  (let [result
        (try (binding [*out* (buffer-writer documentation-buffer)]
               (load-string
                (str "(ns frankentone.live
  (:use frankentone.dsp
        frankentone.ugens
        frankentone.utils
        frankentone.patterns
        frankentone.instruments
        overtone.music.time
        clojure.repl)) \n"
                     \( "doc " symbol \) )))
             (catch Exception e e))]
    (invoke-later
     (when (= (text documentation-buffer) "")
       (text! documentation-buffer (str "No documentation for \"" symbol "\" found."))))
    result))


(defn get-region-boundaries [editor pos]
  (when-let [region (first (let [folds (.getFolds (LispFoldParser.) editor)]
                             (doall (filter
                                     #(do
                                        ;;(println %1 pos)
                                        (.containsOrStartsOnLine %1
                                                                 (.getLineOfOffset
                                                                  editor
                                                                  pos)))
                                     folds))))]
    ;;(println region)
    (list (.getStartOffset region)
          (min (inc (.getEndOffset region)) (count (text editor))))))


(defn a-eval-selection [e]
  (future (when-let [to-eval (.getSelectedText (get-active-editor-tab))]
     (eval-string to-eval))))


(defn a-eval-selection-or-line [e]
  (future (if-let [to-eval (.getSelectedText (get-active-editor-tab))]
     (eval-string to-eval)
     (let [line (.getLineOfOffset (get-active-editor-tab) (.getCaretPosition (get-active-editor-tab)))]
       (eval-string (subs (text (get-active-editor-tab))
                          (.getLineStartOffset (get-active-editor-tab) line)
                          (.getLineEndOffset (get-active-editor-tab) line)
                          ))))))


(defn a-eval-region-or-line [e]
  (future (if-let [to-eval (get-region-boundaries (get-active-editor-tab) (.getCaretPosition (get-active-editor-tab)))]
     (eval-string (apply subs (text (get-active-editor-tab)) to-eval))
     (let [line (.getLineOfOffset (get-active-editor-tab) (.getCaretPosition (get-active-editor-tab)))]
       (eval-string (subs (text (get-active-editor-tab))
                          (.getLineStartOffset (get-active-editor-tab) line)
                          (.getLineEndOffset (get-active-editor-tab) line)
                          ))))))


(defn get-token-at-caret
  ([editor]
     (get-token-at-caret editor 0))
  ([editor offset]
     (let [position (max 0 (min (count (text editor))
                                (+ (.getCaretPosition editor) offset)))
           token (RSyntaxUtilities/getTokenAtOffset
                  (.getTokenListForLine
                   editor
                   (.getLineOfOffset
                    editor
                    position
                    ))
                  position)]
       token)))


(defn a-docstring [e]
  (let [to-look-up (get-token-at-caret (get-active-editor-tab) -1)]
    (when-not (or (not to-look-up)
                  (not (.type to-look-up))
                  (= (.type to-look-up) TokenTypes/NULL))
      (show-documentation (.getLexeme to-look-up)))))

(def menus
  (let [a-new (action :handler a-new :name "New"
                      :tip "Create a new file."
                      :key "menu N")
        a-open (action :handler a-open :name "Open"
                       :tip "Open a file"
                       :key "menu O")
        a-save (action :handler a-save :name "Save"
                       :tip "Save the current file."
                       :key "menu S")
        a-save-as (action :handler a-save-as :name "Save As"
                          :tip "Save the current file."
                          :key "menu shift S")
        a-exit (action :handler a-exit :name "Exit"
                       :tip "Exit the editor.")
        a-copy (action :handler a-copy :name "Copy"
                       :tip "Copy selected text to the clipboard."
                       :key "menu C")
        a-paste (action :handler a-paste :name "Paste"
                        :tip "Paste text from the clipboard."
                        :key "menu V")
        a-cut (action :handler a-cut :name "Cut"
                      :tip "Cut text to the clipboard."
                      :key "menu X")
        a-eval-region-or-line (action :handler a-eval-region-or-line :name "Eval region"
                                 :tip "Evaluate the region which the cursor is in"
                                 :key "control alt X")
        a-eval-selection (action :handler a-eval-selection :name "Eval selection"
                                 :tip "Evaluate the selected text"
                                 :key "menu shift E")
        a-eval-selection-or-line (action :handler a-eval-selection-or-line
                                         :name "Eval selection or line"
                                         :tip "Evaluate the selected text or the current line"
                                         :key "menu E")
        a-docstring (action :handler a-docstring :name "Documentation for symbol"
                            :tip "Show documentation for symbol"
                            :key "menu D")]
    (menubar
     :items [(menu :text "File" :items [a-new a-open a-save a-save-as a-exit])
             (menu :text "Edit" :items [a-copy a-cut a-paste])
             (menu :text "Code" :items [a-eval-selection-or-line
                                        a-eval-selection
                                        a-eval-region-or-line])
             (menu :text "Documentation" :items
                   (conj (mapv (fn [[namespace name]]
                            (menu :text name
                                  :items
                                  (mapv (fn [item]
                                          (action :handler (fn [e]
                                                             (show-documentation (str item)))
                                                  :name (str item)
                                                  :tip (str "Show documentation for " item))
                                          ) (-> namespace ns-publics keys sort))))
                          [['frankentone.dsp "DSP"]
                           ['frankentone.instruments "Instruments"]
                           ['frankentone.patterns "Patterns"]
                           ['frankentone.ugens "UGens"]
                           ['frankentone.utils "Utils"]
                           ['overtone.music.time "Timing"]
                           ])
                         a-docstring))
             (menu :text "Help")])))





(defn run []
  (->   (frame
       :title "Frankentone Editor"
       :content main-panel
       :minimum-size [640 :by 480]
       :menubar menus) pack! show!))


(run)
