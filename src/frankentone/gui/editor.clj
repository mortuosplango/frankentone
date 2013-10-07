;; based on the seesaw text-editor example:
;; https://github.com/daveray/seesaw/blob/develop/test/seesaw/test/examples/text_editor.clj

(ns frankentone.gui.editor
  (:use [seesaw core chooser mig keymap color]
        [frankentone.gui editor-utils]
        [clojure.java.io :only [file resource]])
  (:require [overtone.music time rhythm pitch]
            [seesaw.rsyntax :as rsyntax]
            [seesaw.keystroke :as keystroke]
            [frankentone.dsp :as dsp]
            [frankentone.gui scope]
            [frankentone instruments patterns speech utils]
            [frankentone.entropy entropy])
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
   (org.fife.ui.rsyntaxtextarea.folding LispFoldParser)))


(native!)

(def open-files
  "Holds all open files in a dictionary.

  The key is the path of the file, the value the instance of the file."
  (atom {
         (.getPath (file (System/getProperty "user.home") ".ftscratch"))
         (file (System/getProperty "user.home") ".ftscratch")}))

;; open standard .ftscratch file
(when-not (.exists ^File (val (first @open-files)))
  (spit (val (first @open-files)) ""))


(def editor-tabs
  "A tabbed panel which holds all open documents

  TODO needs a better name"
  (tabbed-panel
   :id :tabs
   :placement :top
   :tabs [ (make-editor-tab (val (first @open-files)))]))


(defn get-active-editor-tab
  "Returns the text area of the currently active editor tab."
   ^RSyntaxTextArea []
  (.getComponent (.getComponent (:content (selection editor-tabs)) 0) 0))


(defn add-editor-tab
  "Adds another editor tab to the editor tabs.

  The tool tip is set to the file name."
  [editor-tab]
  (doto editor-tabs
    (.addTab (:title editor-tab)
             (make-widget (:content editor-tab)))
    (.setToolTipTextAt (dec (.getTabCount editor-tabs)) (:tip editor-tab))
    (.setSelectedIndex (dec (.getTabCount editor-tabs)))))


(defn get-current-file 
  "Returns the file associated with the currently active editor tab."
  ^File []
  (val (find  @open-files
              (.getToolTipTextAt editor-tabs
                                 (.getSelectedIndex editor-tabs)))))


(def post-buffer
  "Text field showing the output of evaluations."
  (text :multi-line? true :font "MONOSPACED-PLAIN-10"
        :text ";;;;; Post buffer ;;;;;; \n \n \n"))


(def documentation-buffer
  "Text field for showing documentation."
  (text :multi-line? true
        :wrap-lines? true
        :font "MONOSPACED-PLAIN-12"
        :text "Welcome to frankentone!"))


(def split-view
  (left-right-split
   editor-tabs
   (top-bottom-split
    (scrollable post-buffer)
    (scrollable documentation-buffer)
    :divider-location 0.5)
   :divider-location 3/5))


(def status-label (label :text ""))
(defn status! [& strings] (text! status-label (apply str strings)))


(def main-panel
  (mig-panel
   :constraints ["fill, ins 0"]
   :items [
           [split-view "grow"]
           [status-label "dock south"]
           [(separator) "dock south"]]))


(defn select-file [type]
  (choose-file main-panel
               :type type
               :dir (.getParent (get-current-file))))


(defn open-file [^File file]
  (if (get @open-files (.getPath file))
    (do (loop [i 0]
          (if (= (.getToolTipTextAt editor-tabs i) (.getPath file))
            (.setSelectedIndex editor-tabs i)
            (when (< (inc i) (.getTabCount editor-tabs))
              (recur (inc i)))))
        (status! (.getName file) " already opened. Tab is now selected."))
    (do (swap! open-files assoc (.getPath file) file)
        (add-editor-tab
         (make-editor-tab file))
        (status! "Opened " file "."))))


(defn eval-string [to-eval]
  (let [result
        (with-out-str-and-value
          (try 
            (load-string
             (str "(use 'frankentone.live) (in-ns 'frankentone.live)" 
                  to-eval))
            (catch Exception e e)))]
    (invoke-later (status! "Result: " (last result))
                  (.append post-buffer
                           (str
                            (when (not= (first result) "")
                              (str (first result) "\n")) 
                            (last result) "\n"))
                  (scroll! post-buffer :to :bottom))
    result))


(defn eval-line [^RSyntaxTextArea editor]
  (let [[start end] (get-line-boundaries
                     editor
                     (.getCaretPosition editor))]
    (eval-string (subs (text editor) start end))
    (flash-region editor start end)))


(defn show-documentation [symbol]
  (let [result
        (with-out-str
          (try 
            (load-string 
             (str "(use 'frankentone.live) (in-ns 'frankentone.live)"
                  \( "doc " symbol \) ))
            (catch Exception e e)))]
    (if (= result "")
      (text! documentation-buffer (str
                                   "No documentation found for "
                                   symbol "."))
      (text! documentation-buffer result))))


;;; all functions prefixed "a-" are actions

(defn a-new [e]
  (let [selected ^File (select-file :save)]
    (if (.exists selected)
      (alert "File already exists.")
      (do
        (swap! open-files assoc (.getPath selected) selected)
        (spit selected "")
        (add-editor-tab
         (make-editor-tab selected))
        (status! "Created a new file.")))))


(defn a-open [e]
  (let [selected (select-file :open)]
    (open-file selected)))


(defn a-save [e]
  (spit (get-current-file) (text (get-active-editor-tab)))
  (status! "Wrote " (get-current-file) "."))


(defn a-close-tab [e]
  (if (> (.getTabCount editor-tabs) 1)
    (invoke-now
     (let [result
           (-> (dialog :content "Save file before closing?"
                       :type :question
                       :resizable? false
                       :option-type :yes-no-cancel)
               pack! show!)]
       (when (= result :success)
         (a-save nil))
       (when result
         (swap! open-files dissoc (.getPath (get-current-file)))
         (.remove editor-tabs (.getSelectedIndex editor-tabs)))))
    (invoke-now
     (alert "Can't close last tab."))))


(defn a-save-as [e]
  (when-let [selected ^File (select-file :save)]
    (swap! open-files dissoc (.getPath (get-current-file)))
    (swap! open-files assoc (.getPath selected) selected)
    (spit selected (text (get-active-editor-tab)))
    (.setTitleAt editor-tabs (.getSelectedIndex editor-tabs) (.getName selected))
    (.setToolTipTextAt editor-tabs (.getSelectedIndex editor-tabs) (.getPath selected))
    (status! "Wrote " selected ".")))


(defn a-exit  [e] (dispose! e))
(defn a-copy  [e] (.copy (get-active-editor-tab)))
(defn a-cut   [e] (.cut (get-active-editor-tab)))
(defn a-paste [e] (.paste (get-active-editor-tab)))
(defn a-comment [e]
  (.actionPerformedImpl
   (RSyntaxTextAreaEditorKit$ToggleCommentAction.) e
   (get-active-editor-tab)))

(defn a-inc-font-size [e]
  (.actionPerformedImpl
   (RTextAreaEditorKit$IncreaseFontSizeAction.) e
   (get-active-editor-tab)))
(defn a-dec-font-size [e]
  (.actionPerformedImpl
   (RTextAreaEditorKit$DecreaseFontSizeAction.) e
   (get-active-editor-tab)))


(defn a-eval-selection-or-line [e]
  (future
    (let [editor (get-active-editor-tab)]
     (if-let [to-eval (.getSelectedText editor)]
       (do (eval-string to-eval)
           (flash-region (.getSelectionStart editor)
                         (.getSelectionEnd editor)
                         (get-active-editor-tab)))
       (eval-line editor)))))


(defn a-eval-region-or-line [e]
  (future
    (let [editor (get-active-editor-tab)]
      (if-let [to-eval (get-region-boundaries
                        editor
                        (.getCaretPosition editor))]
        (do (eval-string (apply subs (text editor) to-eval))
            (flash-region editor (first to-eval) (second to-eval)))
        (eval-line editor)))))


(defn a-start-dsp [e] (eval-string "(start-dsp)"))
(defn a-stop-dsp [e] (eval-string "(stop-dsp)"))


(defn a-stop [e] 
  (overtone.at-at/stop-and-reset-pool! overtone.music.time/player-pool)
  (doall (map  #(.clear (val %))
               @frankentone.instruments/instruments))
  (dsp/silence!))


(defn a-stop-scheduled [e]
  (overtone.at-at/stop-and-reset-pool! overtone.music.time/player-pool))


(defn a-open-source [e]
  (let [editor (get-active-editor-tab)
        to-look-up (get-token-at-caret editor -1)]
    (when (valid-token? to-look-up)
      (if-let [v (ns-resolve 'frankentone.live (symbol (.getLexeme to-look-up)))]
        (if-let [filepath (:file (meta v))]
          (let [sourcefile (file filepath)
                alternative (file (str "src/" filepath))]
            (if (or (.exists sourcefile) (.exists alternative))
              (do (if (.exists sourcefile)
                    (open-file sourcefile)
                    (open-file alternative))
                  (scroll! editor
                           :to [:line (max 0 (dec (:line (meta v))))]))
              (status! "Couldn't open source file " sourcefile
                          " for " (str v) ".")))
          (status! "Couldn't find source for " (str v) "."))
        (status! "Couldn't find source for " (.getLexeme to-look-up) ".")))))


(defn a-scope [e] (frankentone.gui.scope/show-scope))


(defn a-docstring [e]
  (let [to-look-up (get-token-at-caret (get-active-editor-tab) -1)]
    (when-not (or (not to-look-up)
                  (not (.type to-look-up))
                  (= (.type to-look-up) TokenTypes/NULL))
      (show-documentation (.getLexeme to-look-up)))))


(defn a-open-file [path e]
  (open-file (file path)))


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
        a-close-tab (action :handler a-close-tab :name "Close tab"
                            :tip "Close the current tab."
                            :key "menu W")
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
        a-comment (action :handler a-comment :name "Toggle comment"
                      :tip "Comment or uncomment the marked lines. "
                      :key "menu SLASH")
        a-inc-font-size (action :handler a-inc-font-size :name "Increase font size"
                      :tip ""
                      :key "menu PLUS")
        a-dec-font-size (action :handler a-dec-font-size :name "Decrease font size"
                      :tip ""
                      :key "menu MINUS")
        a-eval-region-or-line (action
                               :handler a-eval-region-or-line
                               :name "Evaluate region or line"
                               :tip "Evaluate the region which the cursor is in"
                               :key "menu ENTER")
        a-eval-selection-or-line (action
                                  :handler a-eval-selection-or-line
                                  :name "Evaluate selection or line"
                                  :tip "Evaluate the selected text"
                                  :key "shift ENTER")
        a-start-dsp (action :handler a-start-dsp
                            :name "Start DSP loop")
        a-stop-dsp (action :handler a-stop-dsp
                           :name "Stop DSP loop")
        a-stop (action :handler a-stop
                       :name "Set DSP fn to silence and cancel events"
                       :key "menu PERIOD")
        a-stop-scheduled (action
                          :handler  a-stop-scheduled
                          :name "Cancel scheduled events")
        a-scope (action :handler a-scope
                        :name "Show stethoscope")
        a-docstring (action :handler a-docstring :name "Documentation for symbol"
                            :tip "Show documentation for symbol"
                            :key "menu D")
        a-open-source (action :handler a-open-source :name "Source for symbol"
                              :tip "Look up implementation"
                              :key "menu I")]
    
    (menubar
     :items [(menu :text "File" :items [a-new a-open a-save a-save-as
                                        (separator)
                                        a-close-tab
                                        (separator)
                                        a-exit])
             (menu :text "Edit" :items [a-copy a-cut a-paste
                                        (separator)
                                        a-comment])
             (menu :text "View" :items [a-inc-font-size
                                        a-dec-font-size])
             (menu :text "Code" :items [a-eval-selection-or-line
                                        a-eval-region-or-line
                                        (separator)
                                        a-open-source])
             (menu :text "DSP" :items [a-start-dsp
                                       a-stop-dsp
                                       a-stop
                                       a-stop-scheduled
                                       (separator)
                                       a-scope])
             (menu :text "Documentation" :items
                   (conj (mapv
                          (fn [[namespace name]]
                            (menu
                             :text name
                             :items
                             (mapv (fn [item]
                                     (action
                                      :handler (fn [e]
                                                 (show-documentation (str item)))
                                      :name (str item)
                                      :tip (str "Show documentation for " item)))
                                   (-> namespace ns-publics keys sort))))
                          [['frankentone.dsp "DSP"]
                           ['frankentone.instruments "Instruments"]
                           ['frankentone.patterns "Patterns"]
                           ['frankentone.speech "Speech"]
                           ['frankentone.entropy.entropy "Entropy"]
                           ['frankentone.ugens "UGens"]
                           ['frankentone.utils "Utils"]
                           ['overtone.music.pitch "Overtone->Pitch"]
                           ['overtone.music.time "Overtone->Time"]
                           ['overtone.music.rhythm "Overtone->Rhythm"]
                           ])
                         (separator)
                         a-docstring))
             
             (menu :text "Help"
                   :items (let [expath "src/frankentone/examples/"]
                            [(action :handler
                                     (partial a-open-file
                                              (str expath "getting-started.clj"))
                                     :name "Getting started"
                                     :tip "Open the tutorial.")
                             (action :handler
                                     (partial a-open-file
                                              (str expath "instruments.clj"))
                                     :name "Example instruments"
                                     :tip "Open the example instruments.")
                             (action :handler
                                     (partial a-open-file
                                              (str expath "sampled.clj"))
                                     :name "How to use samples"
                                     :tip "Open the samples example.")
                             (action :handler
                                     (partial a-open-file
                                              (str expath "speech.clj"))
                                     :name "Speech synthesis example")]))])))


(defn run []
  (-> (frame
       :title "Franken[~]tone Editor"
       :content main-panel
       :minimum-size [640 :by 480]
       :menubar menus) pack! show!))

