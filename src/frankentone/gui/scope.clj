(ns frankentone.gui.scope
  (:use [frankentone dsp])
  (:use [seesaw core graphics color]))


(defn make-paint-scope [^java.nio.ShortBuffer b]
  (let [w 256
        h 256
        x-array (int-array (range w))
        y1-array (int-array w)
        y2-array (int-array w)
        kolor (color 0 140 236)
        stroke-t (stroke :width 1.5)
        h4 (int (/ h 4))
        h34 (int (* h 3/4))
        step (int (/ (* 2 *default-buffer-size*) w))
        steps1 (int-array (mapv #(* step %) (range w)))
        steps2 (int-array (mapv #(mod (inc (* step %))
                                      (* 2 *default-buffer-size*))
                                (range w)))
        y-scale (* -1 (/ h4 Short/MAX_VALUE))]
    
    (fn [_
        ^java.awt.Graphics2D g]
      (dotimes [i w]
        (aset ^ints y1-array i
              (unchecked-add-int
               (* (.get b (aget ^ints steps1 i)) y-scale)
               h4))
        (aset ^ints y2-array i
              (unchecked-add-int
               (* (.get b (aget ^ints steps2 i)) y-scale)
               h34)))
      (.setColor g kolor)
      (.setStroke g stroke-t)
      (.drawPolyline g x-array y1-array w)
      (.drawPolyline g x-array y2-array w))))


(defn show-scope
  "Show an oscilloscope view. Only one instance is possible at a time."
  []
  (let [mcanv (canvas :id :canvas
                      :background "#DDDDDD"
                      :paint nil)
        set-mcanv (fn [x]
                        (when-not (nil? x)
                          (.setCallbackFunc x
                                            (fn []
                                              (repaint! mcanv)))
                          (config! mcanv :paint (make-paint-scope
                                                 (.getDspBuf x)))))
        scope-frame (frame 
                     :title "Scope" 
                     :minimum-size [256 :by 280]
                     :content mcanv
                     :on-close :dispose)
        watch-key (keyword (gensym))]
    (set-mcanv @cplay)
    (add-watch cplay watch-key
               (fn [key ref old-state new-state]
                 (set-mcanv new-state)))
    (listen scope-frame :window-closing
            (fn [_] (remove-watch cplay watch-key)))
    (->  scope-frame pack! show!)))

