;; based on the seesaw text-editor example:
;; https://github.com/daveray/seesaw/blob/develop/test/seesaw/test/examples/text_editor.clj

(ns frankentone.gui.editor
  (:use [seesaw core chooser mig keymap color]
        [clojure.java.io :only [file resource]])
  (:require overtone.music.time
            [seesaw.rsyntax :as rsyntax]
            [seesaw.keystroke :as keystroke]
            [frankentone.dsp :as dsp]
            [frankentone.gui scope]
            [frankentone instruments patterns speech]
            [frankentone.entropy entropy])
  (:import
   (java.io Writer)
   (javax.swing.text DefaultEditorKit)
   (java.awt Container)
   (java.io File)
   (java.awt.im InputContext)
   (org.fife.ui.rtextarea ChangeableHighlightPainter
                          RTextScrollPane
                          RTextAreaEditorKit)
   (org.fife.ui.rsyntaxtextarea RSyntaxTextArea
                                RSyntaxTextAreaHighlighter
                                RSyntaxTextAreaEditorKit
                                RSyntaxTextAreaDefaultInputMap
                                RSyntaxUtilities
                                TokenTypes)
   (org.fife.ui.rsyntaxtextarea.folding LispFoldParser)))


(native!)

(defn pimp-editor-keymap [^RSyntaxTextArea editor]
  (let [neo (= (.hashCode (.getLocale (InputContext/getInstance)))
               1921505307)
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
                   ["menu MINUS" "menu alt L"
                    RTextAreaEditorKit/rtaDecreaseFontSizeAction]
                   ["menu PLUS" "menu alt N"
                    RTextAreaEditorKit/rtaIncreaseFontSizeAction]
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


(def open-files
  (atom {
         (.getPath (file (System/getProperty "user.home") ".ftscratch"))
         (file (System/getProperty "user.home") ".ftscratch")}))

(when-not (.exists ^File (val (first @open-files)))
  (spit (val (first @open-files)) ""))

(def current-file-label (label :text "" :font "SANSSERIF-PLAIN-8"))

(defn make-editor-tab [^File file]
  (let [editor-tab (rsyntax/text-area
                    :text file
                    :syntax :clojure
                    :tab-size 2)]
    (pimp-editor-keymap editor-tab)
    {:title (.getName file)
     :tip (.getPath file)
     :content  (RTextScrollPane. editor-tab)}))

(def editor
  (tabbed-panel
   :id :tabs
   :placement :top
   :tabs [ (make-editor-tab (val (first @open-files)))]))

(defn get-active-editor-tab ^RSyntaxTextArea []
  (.getComponent (.getComponent (:content (selection editor)) 0) 0))

(defn add-editor-tab-to-editor [editor-tab]
  (doto editor
    (.addTab (:title editor-tab)
             (make-widget (:content editor-tab)))
    (.setToolTipTextAt (dec (.getTabCount editor)) (:tip editor-tab))
    (.setSelectedIndex (dec (.getTabCount editor)))))

(defn get-current-file ^File []
  (val (find  @open-files
              (.getToolTipTextAt editor
                                 (.getSelectedIndex editor)))))

(def post-buffer
  (text :multi-line? true :font "MONOSPACED-PLAIN-10"
        :text ";;;;; Post buffer ;;;;;; \n \n \n"))


(def documentation-buffer
  (text :multi-line? true
        :wrap-lines? true
        :font "MONOSPACED-PLAIN-12"
        :text "Welcome to frankentone!"))


(def split-view (left-right-split editor
                                  (top-bottom-split
                                   (scrollable post-buffer)
                                   (scrollable documentation-buffer)
                                   :divider-location 0.5)
                                  :divider-location 3/5))


(defmacro with-out-str-and-value
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [v# ~@body]
         (vector (str s#)
                 v#)))))


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


(defn select-file [type] (choose-file main-panel
                                      :type type
                                      :dir (.getParent (get-current-file))))


(defn a-new [e]
  (let [selected ^File (select-file :save)]
    (if (.exists selected)
      (alert "File already exists.")
      (do
        (swap! open-files assoc (.getPath selected) selected)
        (spit selected "")
        (add-editor-tab-to-editor
         (make-editor-tab selected))
        (set-status "Created a new file.")))))


(defn open-file [^File file]
  (if (get @open-files (.getPath file))
    (do (loop [i 0]
          (if (= (.getToolTipTextAt editor i) (.getPath file))
            (.setSelectedIndex editor i)
            (when (< (inc i) (.getTabCount editor))
              (recur (inc i)))))
        (set-status (.getName file) " already opened. Tab is now selected."))
    (do (swap! open-files assoc (.getPath file) file)
        (add-editor-tab-to-editor
         (make-editor-tab file))
        (set-status "Opened " file "."))))


(defn a-open [e]
  (let [selected (select-file :open)]
    (open-file selected)))


(defn a-save [e]
  (spit (get-current-file) (text (get-active-editor-tab)))
  (set-status "Wrote " (get-current-file) "."))


(defn a-close-tab [e]
  (if (> (.getTabCount editor) 1)
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
         (.remove editor (.getSelectedIndex editor)))))
    (invoke-now
     (alert "Can't close last tab."))))


(defn a-save-as [e]
  (when-let [selected ^File (select-file :save)]
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
        (with-out-str-and-value
          (try 
            (load-string
             (str "(use 'frankentone.live) (in-ns 'frankentone.live)" 
                  to-eval))
            (catch Exception e e)))]
    (invoke-later (set-status "Result: " (last result))
                  (.append post-buffer
                           (str
                            (when (not= (first result) "")
                              (str (first result) "\n")) 
                            (last result) "\n"))
                  (scroll! post-buffer :to :bottom))
    result))


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


(defn get-region-boundaries [^RSyntaxTextArea editor pos]
  (when-let [region (first
                     (let [folds (.getFolds (LispFoldParser.) editor)]
                       (doall (filter
                               #(do
                                  (.containsOrStartsOnLine %1
                                                           (.getLineOfOffset
                                                            editor
                                                            pos)))
                               folds))))]
    ;;(println region)
    (list (.getStartOffset region)
          (min (inc (.getEndOffset region)) (count (text editor))))))

(defn get-line-boundaries  [^RSyntaxTextArea editor pos]
  (let [line (.getLineOfOffset editor pos)
        start (.getLineStartOffset editor line)
        end (.getLineEndOffset editor line)]
    (list start end)))

(defn flash-region [^RSyntaxTextArea editor start end] 
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


(defn eval-line [^RSyntaxTextArea editor]
  (let [[start end] (get-line-boundaries
                     editor
                     (.getCaretPosition editor))]
    (eval-string (subs (text editor) start end))
    (flash-region editor start end)))


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


(defn get-token-at-caret
  ([^RSyntaxTextArea editor]
     (get-token-at-caret editor 0))
  ([^RSyntaxTextArea editor offset]
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


(defn a-open-source [e]
  (let [editor (get-active-editor-tab)
        to-look-up (get-token-at-caret editor -1)]
    (when-not (or (not to-look-up)
                  (not (.type to-look-up))
                  (= (.type to-look-up) TokenTypes/NULL))
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
              (set-status "Couldn't open source file " sourcefile
                          " for " (str v) ".")))
          (set-status "Couldn't find source for " (str v) "."))
        (set-status "Couldn't find source for " (.getLexeme to-look-up) ".")))))


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
        a-start-dsp (action :handler (fn [e] (dsp/start-dsp))
                            :name "Start DSP loop")
        a-stop-dsp (action :handler (fn [e] (dsp/stop-dsp))
                           :name "Stop DSP loop")
        a-stop (action :handler (fn [e]
                                  (overtone.at-at/stop-and-reset-pool!
                                   overtone.music.time/player-pool)
                                  (doall (map  #(.clear (val %))
                                               @frankentone.instruments/instruments))
                                  (dsp/silence!))
                       :name "Set DSP fn to silence and cancel events"
                       :key "menu PERIOD")
        a-stop-scheduled (action
                          :handler  (fn [e]
                                      (overtone.at-at/stop-and-reset-pool!
                                       overtone.music.time/player-pool))
                          :name "Cancel scheduled events")
        a-scope (action :handler (fn [e] (frankentone.gui.scope/show-scope))
                        :name "Show stethoscope")
        a-docstring (action :handler a-docstring :name "Documentation for symbol"
                            :tip "Show documentation for symbol"
                            :key "menu D")
        a-open-source (action :handler a-open-source :name "Source for symbol"
                              :tip "Look up implementation"
                              :key "menu I")]
    
    (menubar
     :items [(menu :text "File" :items [a-new a-open a-save a-save-as
                                        a-close-tab
                                        (separator)
                                        a-exit])
             (menu :text "Edit" :items [a-copy a-cut a-paste])
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
                           ['overtone.music.time "Overtone->Time"]
                           ['overtone.music.pitch "Overtone->Pitch"]])
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

