(ns frankentone.entropy.selfmod-test
  (:use clojure.test
        frankentone.entropy.selfmod)
  (:require [seesaw.rsyntax :as rsyntax]
            [frankentone.live]
            [frankentone.gui editor editor-utils]
            [seesaw.core])
  (:import
   (org.fife.ui.rsyntaxtextarea RSyntaxTextArea)))


(deftest selfmod-test
  (let [editor (rsyntax/text-area
                :syntax :clojure
                :tab-size 2
                :text "(+ 1 2)
(defn t88 [] [80 70 60]
  )
(+ 3 4)")]
    (.setCodeFoldingEnabled editor true)
    (.reparse (.getFoldManager editor))
    ;; don't modify if caret is in fn
    (.setCaretPosition editor 16)
    (selfmod-cb editor false "t88" 1 79)
    (is (= (seesaw.core/text editor)
           "(+ 1 2)
(defn t88 [] [80 70 60]
  )
(+ 3 4)"))
    (.setCaretPosition editor 40)
    (selfmod-cb editor false "t88" 1 79000)
    (is (= (seesaw.core/text editor)
           "(+ 1 2)
(defn t88 [] [79000 70 60]
  )
(+ 3 4)"))
    (is (= (.getCaretPosition editor) 43))
    (seesaw.core/selection! editor [0 3])
    (selfmod-cb editor false "t88" 1 790)
    (is (= (seesaw.core/text editor)
           "(+ 1 2)
(defn t88 [] [790 70 60]
  )
(+ 3 4)"))
    (is (= (seesaw.core/selection editor) [0 3]))
    (seesaw.core/selection! editor [39 41])
    (selfmod-cb editor false "t88" 1 7)
    (is (= (seesaw.core/text editor)
           "(+ 1 2)
(defn t88 [] [7 70 60]
  )
(+ 3 4)"))
    (is (= (seesaw.core/selection editor) [37 39]))
    (selfmod-cb editor false "t88" 0 nil)
    (is (= (seesaw.core/text editor)
           "(+ 1 2)
(defn t88 [] nil
  )
(+ 3 4)"))))


(deftest swap-val-test
  (let [editor (rsyntax/text-area
                :syntax :clojure
                :tab-size 2
                :text "(+ 1 2)
(defn t88 [] [80 70 60]
  )
(+ 3 4)")]
   (.setCodeFoldingEnabled editor true)
   (let [[start end] (frankentone.gui.editor-utils/get-region-boundaries
                      editor (.indexOf (seesaw.core/text editor)  "t88"))]
     (is (= (swap-val editor false start end 1 79 3)
            "(defn t88 [] [79 70 60]
  )")))))

;;(seesaw.core/text editor)


