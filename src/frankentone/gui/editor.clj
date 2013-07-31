;; based on the seesaw text-editor example:
;; https://github.com/daveray/seesaw/blob/develop/test/seesaw/test/examples/text_editor.clj

(ns frankentone.gui.editor
  (:use [seesaw core chooser mig]
        [clojure.java.io :only [file resource]])
  (:require overtone.music.time
            [seesaw.rsyntax :as rsyntax]
            [seesaw.keystroke :as keystroke]
            [frankentone.dsp :as dsp]
            [frankentone instruments patterns utils ugens live])
  (:import
   (java.io Writer)
   (javax.swing.text DefaultEditorKit)
   (java.awt Container)
   (java.io File)
   (org.fife.ui.rtextarea RTextScrollPane
                          RTextAreaEditorKit)
   (org.fife.ui.rsyntaxtextarea RSyntaxTextArea
                                RSyntaxTextAreaEditorKit
                                RSyntaxTextAreaDefaultInputMap
                                RSyntaxUtilities
                                TokenTypes)
   (org.fife.ui.rsyntaxtextarea.folding LispFoldParser)))


(native!)

(defn pimp-editor-keymap [^RSyntaxTextArea editor]
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

(when-not (.exists ^File (val (first @open-files))) (spit (val (first @open-files)) ""))

(def current-file-label (label :text "" :font "SANSSERIF-PLAIN-8"))

(defn make-editor-tab [^File file]
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

;;(add-editor-tab-to-editor (make-editor-tab @current-file))

(def post-buffer
  (text :multi-line? true :font "MONOSPACED-PLAIN-10"
        :text ";;;;; Post buffer ;;;;;; \n \n \n"))


(def documentation-buffer
  (text :multi-line? true :font "MONOSPACED-PLAIN-12"
        :text "Welcome to frankentone!"))


(def split-view (left-right-split editor
                                  (top-bottom-split
                                   (scrollable post-buffer)
                                   (scrollable documentation-buffer)
                                   :divider-location 0.5)
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
  (let [selected ^File (select-file :save)]
    (if (.exists selected)
      (alert "File already exists.")
      (do
        (swap! open-files assoc (.getPath selected) selected)
        (spit selected "")
        (add-editor-tab-to-editor
         (make-editor-tab selected))
          ;;(text! (get-active-editor-tab) "")
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
    result))


(defn get-region-boundaries [^RSyntaxTextArea editor pos]
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
  (future (if-let [to-eval (get-region-boundaries
                            (get-active-editor-tab)
                            (.getCaretPosition (get-active-editor-tab)))]
            (eval-string (apply subs (text (get-active-editor-tab)) to-eval))
            (let [line (.getLineOfOffset (get-active-editor-tab)
                                         (.getCaretPosition (get-active-editor-tab)))]
              (eval-string (subs (text (get-active-editor-tab))
                                 (.getLineStartOffset (get-active-editor-tab) line)
                                 (.getLineEndOffset (get-active-editor-tab) line)
                                 ))))))


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
  (let [to-look-up (get-token-at-caret (get-active-editor-tab) -1)]
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
                  (scroll! (get-active-editor-tab)
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
                                        a-eval-selection
                                        a-eval-region-or-line
                                        (separator)
                                        a-open-source])
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
                           ['overtone.music.time "Overtone->Time"]
                           ['overtone.music.pitch "Overtone->Pitch"]
                           ])
                         (separator)
                         a-docstring))
             (menu :text "Help"
                   :items [(action :handler
                                   (fn [e]
                                     (open-file (file
                                                 "src/frankentone/examples/getting-started.clj")))
                                   :name "Getting started"
                                   :tip "Open the tutorial.")
                           (action :handler
                                   (fn [e]
                                     (open-file (file
                                                 "src/frankentone/examples/instruments.clj")))
                                   :name "Example instruments"
                                   :tip "Open the example instruments.")])])))


(defn run []
  (-> (frame
       :title "Frankentone Editor"
       :content main-panel
       :minimum-size [640 :by 480]
       :menubar menus) pack! show!))

