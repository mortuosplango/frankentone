(require 'leiningen.core.eval)

(def JVMOPTS
  "Per os jvm options. Options common to all cases go under
`:any`. Options specific to one OS go under the key returned by
`leiningen.core.eval/get-os` for that system. Temporarily disabled
options can be kept under `:disabled`."
  {:any
   ["-Xms512m" "-Xmx1g" ; Minimum and maximum sizes of the heap
    "-XX:+UseParNewGC" ; Use the new parallel GC in conjunction with
    "-XX:+UseConcMarkSweepGC" ; the concurrent garbage collector
    "-XX:+CMSConcurrentMTEnabled" ; Enable multi-threaded concurrent gc work (ParNewGC)
    "-XX:MaxGCPauseMillis=20" ; Specify a target of 20ms for max gc pauses
    "-XX:+CMSIncrementalMode" ; Do many small GC cycles to minimize pauses
    "-XX:MaxNewSize=257m" ; Specify the max and min size of the new
    "-XX:NewSize=256m" ; generation to be small
    "-XX:+UseTLAB" ; Uses thread-local object allocation blocks. This
                                  ; improves concurrency by reducing contention on
                                  ; the shared heap lock.
    "-XX:MaxTenuringThreshold=0"] ; Makes the full NewSize available to every NewGC
                                  ; cycle, and reduces the pause time by not
                                  ; evaluating tenured objects. Technically, this
                                  ; setting promotes all live objects to the older
                                  ; generation, rather than copying them.
   :macosx
   ["-Xdock:name=franken[~]tone"]
   :disabled
   ["-XX:ConcGCThreads=2" ; Use 2 threads with concurrent gc collections
    "-XX:TieredCompilation" ; JVM7 - combine both client and server compilation
                                  ; strategies
    "-XX:CompileThreshold=1" ; JIT each function after one execution
    "-XX:+PrintGC" ; Print GC info to stdout
    "-XX:+PrintGCDetails" ; - with details
    "-XX:+PrintGCTimeStamps"]}) ; - and timestamps

(defn jvm-opts
  "Return a complete vector of jvm-opts for the current os."
  [] (let [os (leiningen.core.eval/get-os)]
       (vec (set (concat (get JVMOPTS :any)
                         (get JVMOPTS os))))))

(defproject frankentone "0.2.0-SNAPSHOT"
  :description "Experimental library for live coding."
  :url "https://github.com/mortuosplango/frankentone"
  :license {:name "GNU General Public License 3"
            :url "https://www.gnu.org/licenses/gpl.txt"}
  :repositories {"local" "file:repo"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojars.wmealing/clj-audio "0.2.0-SNAPSHOT"]
                 [org.jaudiolibs/audioservers-api "1.1.0"]
                 [org.jaudiolibs/audioservers-jack "1.1.0"]
                 [org.jaudiolibs/audioservers-javasound "1.1.0"]
                 [hiphip-aot "0.1.2"]
                 [seesaw "1.4.4"]
                 [overtone "0.9.1"]
                 [org.clojars.jeffsigmon/maryclient "4.3.0"]
                 [ddf.minim "2.1.0"]
                 [incanter/incanter-core "1.5.5"]
                 [incanter/incanter-charts "1.5.5"]]
  :jvm-opts ~(jvm-opts)
  :main frankentone.core)
