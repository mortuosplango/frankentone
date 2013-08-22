;; based on https://github.com/LAMF/bloodymary

(ns frankentone.speech
  (:import
   (marytts.client MaryClient MaryClient$AudioPlayerListener)
   (marytts.client.http Address)
   (marytts.util.data.audio AudioPlayer)
   (java.io ByteArrayInputStream ByteArrayOutputStream)))


(def ^:dynamic *host* "localhost")
(def ^:dynamic *port* 59125)
(def ^:dynamic *input-type* "TEXT")
(def ^:dynamic *audio-type* "AU")
(def ^:dynamic *locale* "en_GB")
(def ^:dynamic *voice-name* "dfki-spike-hsmm")
(def ^:dynamic *style* "")
(def ^:dynamic *effects* "")

  
(defn- get-client [host port]
  (MaryClient/getMaryClient (Address. host port)))


(defn play-audio
  "Render and play string s directly."
  [s & options]
  (let [opts (apply hash-map options)]
    (.streamAudio
     (get-client
      (get opts :host *host*)
      (get opts :port *port*))
     s
     (get opts :input-type *input-type*)
     (get opts :locale *locale*)
     (get opts :audio-type *audio-type*)
     (get opts :voice-name *voice-name*)
     (get opts :style *style*)
     (get opts :effects *effects*)
     (AudioPlayer.)
     (proxy [MaryClient$AudioPlayerListener] []))))


(defn get-audio
  "Renders string s and returns a byte-array with the audio data in the
  format specified by *audio-type*"
  ^bytes [s & options]
  (let [stream (ByteArrayOutputStream.)
        opts (apply hash-map options)
        client (get-client
                (get opts :host *host*)
                (get opts :port *port*))]
    (.process
     client s
     (get opts :input-type *input-type*)
     "AUDIO"
     (get opts :locale *locale*)
     (get opts :audio-type *audio-type*)
     (get opts :voice-name *voice-name*)
     stream)
    (.toByteArray stream)))


(def mem-get-audio
  "Memoized version of get-audio."
  (memoize get-audio))


(defn render-string
  "Renders string s, converts it and returns a double-array of the
  audio data.

  Use mem-render-string for a memoized version to avoid unnecessary
  rerendering and converting."
  ^doubles [string]
  (let [spc (get-audio string)
        shorts (short-array (/ (count spc) 2))
        scaling-factor (/ 1.0 Short/MAX_VALUE)]
    (.get
     (.asShortBuffer
      (.order
       (java.nio.ByteBuffer/wrap spc) java.nio.ByteOrder/BIG_ENDIAN))
     shorts)
    (double-array (map #(* % scaling-factor) (drop 16 shorts)))))


(def mem-render-string
  "Memoized version of render-string."
  (memoize render-string))
