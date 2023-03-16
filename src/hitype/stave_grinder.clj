(ns hitype.stave-grinder
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.buffered-image :as buffered-image]
;;            [fungl.component.text-area :as text-area]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.util :as util]
            [hitype.util :as hitype-util]
            [flow-gl.gui.keyboard :as keyboard]
            [fungl.cache :as cache]
            [clojure.test :refer :all]
            [clojure.set :as set]

            [clojure.xml :as xml]
            [medley.core :as medley]

            [clojure.core.async :as async]

            [overtone.midi :as midi]
            [overtone.midi.file :as midi-file]

            )
  (:import java.util.concurrent.PriorityBlockingQueue
           ;;           [org.apache.commons.net.ntp TimeStamp]
           (javax.sound.midi Sequencer Synthesizer
                             MidiSystem MidiDevice Receiver Transmitter MidiEvent
                             MidiMessage ShortMessage SysexMessage
                             InvalidMidiDataException MidiUnavailableException
                             MidiDevice$Info)
           ))

(comment
  (let [synthesizer (MidiSystem/getSynthesizer)]
    (.open synthesizer)
    (let [instruments (.getInstruments (.getDefaultSoundbank synthesizer))
          channels (.getChannels synthesizer)]
      (.loadInstrument synthesizer
                       (first instruments))
      (.noteOn (first channels) 60 100)
      (Thread/sleep 1000)
      (.noteOff (first channels)
                60)))
  (midi/midi-note-on  40 100)
  ) ;; TODO: remove-me



(defn launchpad-device []
  (medley/find-first (fn [device]
                       (= "Launchpad X LPX MIDI Out"
                          (:description device)))
                     (midi/midi-sources)))

(comment
  (midi/midi-ports)
  (launchpad-device)
  ) ;; TODO: remove-me

(defonce pressed-keys (dependable-atom/atom #{}))
(comment
  (reset! pressed-keys #{})
  @pressed-keys
  ) ;; TODO: remove-me


(defn text [string & [size color]]
  (visuals/text (str string)
                 {:color (if color
                           color
                           [0 0 0 255])
                  :font-size (or size 50)})
  #_(visuals/text-area (str string)
                       (if color
                         color
                         [0 0 0 255])
                       (visuals/liberation-sans-regular (or size 50))))


(defn note-events [track]
  (->> (:events track)
       (filter (fn [event]
                 (contains? #{:note-on :note-off}
                            (:command event))))))

(defn combine-note-events [note-events]
  (->> note-events
       (sort-by :timestamp)
       (partition 2)
       (map (fn [[on off]]
              {:start (:timestamp on)
               :end (:timestamp off)
               :pitch (:note on)}))))

(defn track-notes [track]
  (->> (note-events track)
       (group-by :note)
       (medley/map-vals combine-note-events)
       (vals)
       (apply concat)
       (sort-by :start)))


(defn tracks [midi-file-name]
  (let [parsed-midi-file (midi-file/midi-file midi-file-name)
        resolution (.getResolution (:sequence parsed-midi-file))
        timestamp-to-beats #(float (/ % resolution))]


    (->> (:tracks parsed-midi-file)
         (map track-notes)
         (map (fn [notes]
                (map (fn [note]
                       (-> note
                           (update :start timestamp-to-beats)
                           (update :end timestamp-to-beats)))
                     notes))))))

(comment
  (float (/ ))
  (tracks "/Users/jukka/google-drive/jukka/music/testi.mid")
  (tracks "/Users/jukka/Downloads/Europe_-_The_Final_Countdown.mid")

  (count (:tracks (midi-file/midi-file "/Users/jukka/Downloads/Europe_-_The_Final_Countdown.mid")))
  (midi-file/midi-file "/Users/jukka/google-drive/jukka/music/testi.mid")

  (->> "/Users/jukka/Downloads/The_Final_Countdown_-_Piano.mid"
       midi-file/midi-file
       :tracks
       first
       track-notes)

  (.getDivisionType (:sequence ))

  ) ;; TODO: remove-me

(def middle-c 60)
(def sexteenth-note-width 40)
(def line-width 2)
(def line-gap (/ sexteenth-note-width 1.4))
(def row-height (* 5 line-gap))
(def middle-finger-shade 230)
(def note-name-shade 40)
(def out-of-scale-shade 170)
(def gray-line-shade 240)
(def fat-line-shade 30)
(def note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])
(def flat-note-names ["C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"])
(def major [2 2 1 2 2 2 1])
(def minor [2 1 2 2 1 2 2])

(def note-index-to-name (into {} (map-indexed vector note-names)))
(def note-index-to-flat-name (into {} (map-indexed vector flat-note-names)))
(def note-name-to-index (into {} (map vec (map reverse (map-indexed vector note-names)))))
(def flat-note-name-to-index (into {} (map vec (map reverse (map-indexed vector flat-note-names)))))

(defn note-name [note]
  (get note-names (dec note)))

(defn note-in-scale [root-index scale n]
  (+ root-index
     (->> scale
          repeat
          (apply concat)
          (take n)
          (reduce +))))

(defn notes-in-scale [root-index scale]
  (map (comp #(mod % 12)
             (partial note-in-scale root-index scale))
       (range 7)))

(deftest test-notes-in-scale
  (is (= '(0 2 3 5 6)
         (notes-in-scale 0 5 [2 1])))

  (is (= '("C" "D" "E" "F" "G" "A" "B")
         (map note-index-to-flat-name
              (notes-in-scale (get note-name-to-index
                                   "C")
                              major))))

  (is (= '("A" "B" "C" "D" "E" "F" "G")
         (map note-index-to-flat-name
              (notes-in-scale (get note-name-to-index
                                   "A")
                              minor)))))

(defn in-scale? [scale pitch]
  (contains? (set (notes-in-scale 0 scale))
             (mod pitch 12)))

(defn in-key? [scale root-index pitch]
  (contains? (set (notes-in-scale root-index
                                  scale))
             (mod (- pitch middle-c)
                  12)))

(deftest test-in-key?
  (is (= true
         (in-key? major
                  (note-name-to-index "C")
                  middle-c)))

  (is (= false
         (in-key? major
                  (note-name-to-index "C")
                  (+ 1 middle-c))))

  (is (= true
         (in-key? major
                  (note-name-to-index "C")
                  (+ 2 middle-c)))))

(defn octave-number [pitch]
  (dec (quot pitch 12)))

(defn note [pitch]
  (mod pitch 12))

(defn pitch [octave note]
  (dec (+ (* 12 octave)
          note)))

(defn chord [number]
  (->> (range 1 8)
       (cycle)
       (drop (dec number))
       (partition 2)
       (map first)
       (take 3)))

(deftest test-chord
  (is (= '((1 3 5) (2 4 6) (3 5 7) (4 6 1) (5 7 2) (6 1 3) (7 2 4))
         (map chord (range 1 8)))))

(defn note-number-to-note [scale note-number]
  (inc (reduce + (take (dec note-number)
                       scale))))

(deftest test-note-number-to-note
  (is (= 1
         (note-number-to-note [1 2 3]
                              1)))

  (is (= 2
         (note-number-to-note [1 2 3]
                              2)))

  (is (= 4
         (note-number-to-note [1 2 3]
                              3)))

  (is (= 7
         (note-number-to-note [1 2 3]
                              4))))

(defn chord-notes [key scale chord-number]
  (->> (chord chord-number)
       (map (partial note-number-to-note scale))
       (map #(+ (dec key)
                %))))

(deftest test-chord-notes
  (is (= '("C" "E" "G")
         (map note-name (chord-notes 1 major 1))))

  (is (= '("D" "F" "A")
         (map note-name (chord-notes 1 major 2))))

  (is (= '("D" "F#" "A")
         (map note-name (chord-notes 3 major 1)))))

(defn line [pitch]
  (layouts/superimpose (let [number-in-row (mod (+ pitch 0) 6)
                             fat? (contains? ;;  #{ 1 2 3 4 }
                                   #{ 0 }
                                   number-in-row)
                             shaded-background? (not (in-scale? major pitch))
                             #_(contains? #{1}
                                          #_#{0 2}
                                          number-in-row)
                             line-width (if fat?
                                          (* 1 line-width)
                                          line-width)]
                         (layouts/superimpose
                          (assoc (visuals/rectangle-2 :fill-color (if fat?
                                                                    [fat-line-shade
                                                                     fat-line-shade
                                                                     fat-line-shade
                                                                     255]
                                                                    [gray-line-shade
                                                                     gray-line-shade
                                                                     gray-line-shade
                                                                     255]))
                                 :y line-gap
                                 :height line-width)
                          (when (#{2 3} number-in-row)
                            (assoc (visuals/rectangle-2 :fill-color [middle-finger-shade
                                                                     middle-finger-shade
                                                                     middle-finger-shade
                                                                     255])
                                   :height line-gap))
                          (when shaded-background?
                              (assoc (visuals/rectangle-2 :fill-color [out-of-scale-shade
                                                                       out-of-scale-shade
                                                                       out-of-scale-shade
                                                                       255])
                                     :height line-gap
                                     :width 30))))))

(defn quantisize [resolution length]
  (* resolution
     (Math/round
      (float (/ length resolution)))))

(deftest test-quantisize
  (is (= 10 (quantisize 10 14)))
  (is (= 20 (quantisize 10 15)))
  (is (= 0.5 (quantisize 0.5 0.7)))
  (is (= 1.0 (quantisize 0.5 0.8))))

(defn quantisize-note [resolution note]
  (-> note
      (update :start (partial quantisize resolution))
      (update :end (partial quantisize resolution))))

(defn note-view [note]
  (assoc (visuals/rectangle-2 :fill-color (if (= 1 (:number-in-scale note))
                                            [200 100 100 255]
                                            [0 0 0 255]))
         :x (* (:start note)
               (* 4 sexteenth-note-width))
         :y (- (* (- (:pitch note)
                     middle-c)
                  line-gap))
         :width (- (* (- (:end note)
                           (:start note))
                      (* 4 sexteenth-note-width))
                   2)
         :height line-gap))


(defn gray [shade]
  [shade shade shade 255])

(defn gap [pad-width]
  (/ pad-width 30))

(defn scale-mark-width [pad-width]
  (/ (- pad-width
        (gap pad-width))
     3))

(defn note-mark-coordinate [pad-width pad-coordinate]
  (- (+ (/ (gap pad-width) 2)
        (/ (- pad-width
              (gap pad-width))
           2)
        (* pad-coordinate pad-width))
     (/ (scale-mark-width pad-width)
        2)))

(def pad-width 120)

(def default-grid-options {:first-pitch (+ 12 6)
                           :row-interval 6
                           :row-count 8
                           :column-count 25})

(defn pitch-highlights [pitches & [options]]
  (let [pitch-set (set pitches)
        {:keys [row-count column-count row-interval first-pitch]} (merge default-grid-options
                                                                         options)
        gap (gap pad-width)
        scale-mark-width (scale-mark-width pad-width)]

    (layouts/superimpose
     (for [y (range row-count)
           x (range column-count)]

       (let [row-from-bottom (inc (- row-count y))
             pitch (+ first-pitch
                      x
                      (* row-from-bottom row-interval))]
         (when (contains? pitch-set
                          pitch)
           (let [highlight-line-width 4
                 highlight-width (+ (* 2 (+ gap highlight-line-width))
                                    scale-mark-width)]
             (merge (assoc (visuals/rectangle-2 :draw-color (or (:draw-color options)
                                                                [0 0 255 255])
                                                :line-width 4
                                                :fill-color (:fill-color options))
                           :x (- (note-mark-coordinate pad-width x)
                                 gap
                                 highlight-line-width)
                           :y (- (note-mark-coordinate pad-width y)
                                 gap
                                 highlight-line-width)
                           :width  highlight-width
                           :height highlight-width)))))))))

(defn grid [& [options]]
  (let [gap (gap pad-width)
        scale-mark-width (scale-mark-width pad-width)
        half-gap (/ gap 2)
        {:keys [row-count column-count row-interval first-pitch]} (merge default-grid-options
                                                                         options)]
    (layouts/superimpose
     (assoc (visuals/rectangle-2 :fill-color (gray 180))
            :width (* column-count pad-width)
            :height (* row-count pad-width))
     (for [y (range row-count)
           x (range column-count)]

       (let [row-from-bottom (inc (- row-count y))
             pitch (+ first-pitch
                      x
                      (* row-from-bottom row-interval))]
         (layouts/superimpose
          (assoc (visuals/rectangle-2 :fill-color #_(gray 255)
                                      (if (= 0 (mod (octave-number pitch) 2))
                                        (gray 255)
                                        (gray 230)))
                 :x (+ half-gap
                       (* x pad-width))
                 :y (+ half-gap
                       (* y pad-width))
                 :width  (- pad-width
                            gap)
                 :height (- pad-width
                            gap))
          (when (in-scale? major pitch)
            (assoc (visuals/rectangle-2 :fill-color (if (= 0 (note pitch))
                                                      [200 100 100 255]
                                                      (gray 200)))
                   :x (note-mark-coordinate pad-width x)
                   :y (note-mark-coordinate pad-width y)
                   :width  scale-mark-width
                   :height scale-mark-width))
          (assoc (text (str (get note-names
                                 (note pitch))
                            #_(octave-number pitch))
                       (/ pad-width 5)
                       (gray 140)
                       )
                 :x (+ 2
                       half-gap
                       (* x pad-width))
                 :y (+ 2
                       half-gap
                       (* y pad-width)))))))))



(defn chord-grid [key scale chord-number]
  (let [row-interval 5]
    (layouts/superimpose
     (grid {:row-interval row-interval})
     (pitch-highlights (let [chord-notes (chord-notes key scale chord-number)
                             octave-notes (fn [octave]
                                            (map (partial pitch octave)
                                                 chord-notes))]
                         (mapcat octave-notes (range 8)))
                       {:row-interval row-interval})
     (pitch-highlights (let [first-chord-note (first  (chord-notes key scale chord-number))]
                         (map (fn [octave]
                                (pitch octave first-chord-note))
                              (range 8)))
                       {:fill-color [0 255 0 255]
                        :row-interval row-interval}))))

(defn chord-view []
  (let [scale 0.5]
    (layouts/flow
     (for [chord (range 1 8)]
       (layouts/with-margins 10 10 10 10
         (layouts/scale scale scale
                       [chord-grid 1 major chord])))))

  #_(layouts/superimpose
     [chord-grid 1 major 1]
     (pitch-highlights @pressed-keys
                       {:fill-color [255 0 0 255]})))

(defn measure [minimum-pitch maximum-pitch]
  (layouts/with-maximum-size (* 16 sexteenth-note-width) nil
    (layouts/superimpose (for [pitch (range minimum-pitch
                                            (inc maximum-pitch))]
                           (assoc (line pitch)
                                  :y (* (- maximum-pitch
                                           pitch)
                                        line-gap)))
                         (assoc (visuals/rectangle-2 :fill-color [0 0 0 255])
                                :x (dec (* sexteenth-note-width 16))
                                :width line-width
                                :height (* line-gap
                                           (- maximum-pitch
                                              minimum-pitch)))
                         (for [pitch (range minimum-pitch
                                              (inc maximum-pitch))]
                             (-> (text (str (get note-names
                                                 (note pitch))
                                            (if (= 0 (note pitch))
                                              (octave-number pitch)))
                                       (* line-gap 0.9)
                                       [note-name-shade note-name-shade note-name-shade 255])
                                 (assoc :y
                                        (+ 1
                                           (* line-gap
                                              (- maximum-pitch
                                                 pitch)))))))))

(comment
  (quot 10 5)
  ) ;; TODO: remove-me

(defn measures [measure-count minimum-pitch maximum-pitch]
  (layouts/horizontally-2 {}
                          (repeat measure-count
                                  [measure minimum-pitch
                                   maximum-pitch]))  )
(defn stave [notes]
  (let [minimum-pitch (- middle-c (* 2 12)) #_(apply min (map :pitch notes))
        maximum-pitch (+ middle-c (* 2 12)) #_(apply max (map :pitch notes))
        first-note-start-time (:start (first notes))
        measure-count 4 #_(/ (- (apply max (map :end notes))
                                first-note-start-time)
                             4)]
    (layouts/superimpose [measures measure-count minimum-pitch maximum-pitch ]
                         #_(for [measure-number (range measure-count)]
                             (assoc (measure minimum-pitc´h
                                             maximum-pitch)
                                    :x (* measure-number (* 16 sexteenth-note-width))))
                         (assoc (layouts/superimpose (map note-view notes))
                                :y (* (- maximum-pitch middle-c)
                                      line-gap)
                                :x (- (* (quot first-note-start-time 4)
                                         16
                                         sexteenth-note-width)))
                         (for [pressed-pitch @pressed-keys]
                           (assoc (visuals/rectangle-2 :fill-color [0 0 200 55])
                                  :y (- (* (- maximum-pitch middle-c)
                                           line-gap)
                                        (* (- pressed-pitch middle-c)
                                           line-gap))
                                  :width (* measure-count sexteenth-note-width 18)
                                  :height line-gap)))))

#_(def notes
  #_(->> (tracks "/Users/jukka/Downloads/The_Final_Countdown_-_Piano.mid"
                        #_"/Users/jukka/google-drive/jukka/music/testi.mid")
                (drop 0)
                (first)
                #_(map (partial quantisize-note
                                (/ 1 8)))
                (take-while (fn [note]
                              (< (:start note)
                                 (* 6 4)))))



  )

(def notes (dependable-atom/atom (->> (tracks #_"/Users/jukka/Downloads/tassako_taa_oli.mid"
                                              #_"/Users/jukka/google-drive/jukka/music/Believer_-_Imagine_Dragons_Advanced.mid"
                                              #_"/Users/jukka/google-drive/jukka/music/Geometry_dash_-_Electroman_adventures.mid"
                                              "/Users/jukka/google-drive/jukka/music/Nausica_of_the_Valley_of_the_Wind_-_The_Days_Long_Gone__Nausicas_Theme.mid"
                                              #_"/Users/jukka/Downloads/IRON_MAN_02.mid"
                                              #_"/Users/jukka/google-drive/jukka/music/Hatsune_Miku_Ivean_Polkka.mid"
                                              #_"/Users/jukka/google-drive/jukka/music/The_Final_Countdown_-_Piano.mid"
                                              #_"/Users/jukka/google-drive/jukka/music/Geometry_Dash_Medley.mid"
                                              #_"/Users/jukka/google-drive/jukka/music/testi.mid")
                                      (drop 0)
                                      (first)
                                      #_(map (partial quantisize-note
                                                      (/ 1 8))))))

(defonce channel
  (let [synthesizer (MidiSystem/getSynthesizer)]
    (.open synthesizer)
    (let [instruments (.getInstruments (.getDefaultSoundbank synthesizer))]
      (.loadInstrument synthesizer
                       (first instruments))
      (first (.getChannels synthesizer)))))

(comment
  
  (midi/midi-handle-events (midi/midi-in "LinnStrument MIDI")
                           #'handle-midi-message)
  (midi/midi-handle-events (midi/midi-in  "Launchpad X LPX MIDI Out")
                           #'handle-midi-message)

  (midi/midi-handle-events (midi/midi-in  "Launchpad X LPX MIDI Out")
                           (fn [_messsage]))
  ) ;; TODO: remove-me




(defn notes-by-bar [notes]
  (into {}
        (for [bar-number (range (Math/ceil (/ (apply max (map :end notes))
                                              4)))]
          [bar-number (filter (fn [note]
                                (and (<= (:start note)
                                         (* (inc bar-number) 4))
                                     (>= (:end note)
                                         (* bar-number 4))))
                              notes)])))

(deftest test-notes-by-bar
  (is (= '{0 ({:start 1, :end 2})}
       (notes-by-bar [{:start 1
                       :end 2}])))

  (is (= '{0 ({:start 1, :end 5}),
           1 ({:start 1, :end 5})}
       (notes-by-bar [{:start 1
                       :end 5}]))))

(defn bar-number [time]
  (int (/ time 4)))

(deftest test-bar-number
  (is (= 0
         (bar-number 0)))

  (is (= 0
         (bar-number 1)))

  (is (= 0
         (bar-number 3)))

  (is (= 1
         (bar-number 4))))

(defn playing-notes [notes-by-bar time]
  (filter (fn [note]
            (and (<= (:start note)
                     time)
                 (>= (:end note)
                     time)))
          (get notes-by-bar
               (bar-number time))))

(deftest test-playing-notes
  (is (= '({:start 1, :end 2})
         (playing-notes {0 [{:start 1, :end 2}]}
                        1))))

(defn midi-change-events [previous-playing-notes-set current-playing-notes-set]
  (concat (for [stopped-note (set/difference previous-playing-notes-set current-playing-notes-set)]
            {:pitch (:pitch stopped-note)
             :command :note-off})
          (for [started-note (set/difference current-playing-notes-set
                                             previous-playing-notes-set)]
            {:pitch (:pitch started-note)
             :command :note-on})))

(defn piano-note-on [note]
  (.noteOn channel
           note
           100))

(defn piano-note-off [note]
  (.noteOff channel
           note))

(defn- beat-time-now [play-start-time beats-per-minute]
  (float (* (/ (- (System/currentTimeMillis)
                  play-start-time)
               1000 60)
            beats-per-minute)))

(defn play-notes [beats-per-minute start-time end-time notes]
  (let [notes-by-bar (notes-by-bar notes)
        last-bar (apply max (keys notes-by-bar))
        stop-atom (atom false)
        playing-notes-atom (atom #{})
        play-start-time (System/currentTimeMillis)
        beat-time-now (fn []
                        (+ start-time
                           (beat-time-now play-start-time beats-per-minute)))
        synth-nodes-atom (atom {})]
    ;; (prn 'last-bar last-bar) ;; TODO: remove-me
    ;; (prn 'start-time start-time) ;; TODO: remove-me
    ;; (prn 'stop-atom stop-atom) ;; TODO: remove-me

    (.start (Thread. (fn []
                       (while (and (not @stop-atom)
                                   (>= end-time
                                       (beat-time-now))
                                   #_(>= last-bar
                                       (Math/ceil (/ (beat-time-now)
                                                     4))))

                         (let [playing-notes-set (set (playing-notes notes-by-bar
                                                                     (beat-time-now)))]

                           (doseq [midi-event (midi-change-events @playing-notes-atom
                                                                  playing-notes-set)]
                             (if (= :note-on (:command midi-event))
                               (piano-note-on (:pitch midi-event))
                               (piano-note-off (:pitch midi-event))))
                           (reset! playing-notes-atom playing-notes-set))
                         (Thread/sleep 10)))))
    stop-atom))

(comment
  (midi/midi-sinks)
  (midi/midi-note-off)
  (do (.start (Thread. (fn []
                         (Thread/sleep 100)
                         (prn "moi"))))
      (prn 'started))

  (notes-by-bar (take 40 @notes))

  (let [stop-atom (play-notes 90
                              6
                              8
                              @notes
                              #_[{:start 1
                                  :end 2
                                  :pitch middle-c}
                                 {:start 1
                                  :end 2
                                  :pitch (+ 3 middle-c)}])]
    (Thread/sleep 10000)
    (reset! stop-atom true))

  (live/stop)
  ) ;; TODO: remove-me

(defn- stop-playing [state-atom]
  (let [state @state-atom]
    (swap! state-atom assoc
           :playing? false
           :play-head-position (+ (:play-head-position state)
                                  (beat-time-now (:play-start-time state)
                                                 (:beats-per-minute state))))
    (reset! (:stop-atom state)
            true)))

(defn handle-keyboard-event [state-atom _scene-graph event]


  (when (= :key-pressed (:type event))
    (cond (= :up (:key event))
          (swap! state-atom update :beats-per-minute #(+ % 10))

          (= :down (:key event))
          (swap! state-atom update :beats-per-minute #(- % 10))

          (= :right (:key event))
          (swap! state-atom update :first-bar-in-view inc)

          (= :left (:key event))
          (swap! state-atom update :first-bar-in-view dec)

          (= :enter (:key event))
          (do (stop-playing state-atom)
              (swap! state-atom assoc :play-head-position 0))

          (= :space (:key event))
          (let [state @state-atom]
            #_(prn (select-keys state
                                [:playing?
                                 :stop-atom
                                 :play-start-time
                                 :beats-per-minute
                                 ])) ;; TODO: remove-me

            (if (:playing? state)
              (stop-playing state-atom)

              (let [stop-atom (play-notes (:beats-per-minute state)
                                          (+ (:play-head-position state)
                                             (* 4 (:first-bar-in-view state)))
                                          (+ (:play-head-position state)
                                             (* 4 (+ 6 (:first-bar-in-view state))))

                                          ;; (:play-head-position state)
                                          ;; (+ (:play-head-position state)
                                          ;;    3)
                                          @notes)]
                (swap! state-atom assoc
                       :stop-atom stop-atom
                       :playing? true
                       :play-start-time (System/currentTimeMillis)))))
          )))

#_(defn handle-keyboard-event [state-atom _scene-graph event]
  (#'handle-keyboard-event2 state-atom event))

(defn note-number [x y]
  (+ x (* y 8)))

(defn launchpad-view []
  (let [grid-size 20]
    (apply layouts/superimpose

           (for [y (range 8)
                 x (range 8)]
             (let [pitch (note-number x y)]
               (assoc (visuals/rectangle-2 :fill-color (if (in-scale? major pitch)
                                                         [255 255 255 255]
                                                         [155 155 155 255])
                                           (nth note-names
                                                (mod pitch
                                                     12)))
                      :x (* grid-size x)
                      :y (* grid-size y)
                      :width grid-size
                      :height grid-size))))))

(defn base-view [state-atom]
  (let [state @state-atom
        notes-in-view (->> @notes
                           (drop-while (fn [note]
                                         (< (:start note)
                                            (* 4 (:first-bar-in-view state)))))
                           (take-while (fn [note]
                                         (< (:start note)
                                            (* 4 (+ (:first-bar-in-view state)
                                                    4))))))]
    (prn 'state state) ;; TODO: remove me

    (when (:playing? state)
      (animation/swap-state! animation/set-wake-up 1)
      @animation/state-atom)

    (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                         (layouts/vertically-2
                          {:margin 10}
                          (layouts/superimpose
                           [stave notes-in-view
                            #_(for [n (range 8)]
                                (let [note (note-in-scale 0 major n)]
                                  {:pitch note
                                   :number-in-scale (inc (mod n 7))
                                   :start (* 2 n)
                                   :duration 2}))]
                           (assoc (visuals/rectangle-2 :fill-color [0 0 200 55])
                                  :x (* (+ (:play-head-position state)
                                           (if (:playing? state)
                                             (beat-time-now (:play-start-time state)
                                                            (:beats-per-minute state))
                                             0))
                                        (* sexteenth-note-width 4))
                                  :width 4
                                  :height (* 50 line-gap)))
                          (text (pr-str (:beats-per-minute state)))))))

(defn ui []
  (let [state-atom (dependable-atom/atom {:first-bar-in-view 0
                                          :play-head-position 1
                                          :beats-per-minute 60})]
    (keyboard/set-focused-event-handler! [#'handle-keyboard-event state-atom])
    (fn []
      (#'base-view state-atom))))

(defonce event-channel-atom (atom nil))

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (reset! event-channel-atom (application/start-application ;; #'ui
;;                              #'grid
                              #'chord-view
                              :on-exit #(reset! event-channel-atom nil)
                              :do-profiling true)))


(when @event-channel-atom
  (async/>!! @event-channel-atom {:type :redraw}))

(comment
  (.getLatency (MidiSystem/getSynthesizer))
  ) ;; TODO: remove-me

(defn handle-midi-message [midi-message]
  (prn midi-message) ;; TODO: remove-me

  (cond (= :note-on (:command midi-message))
        (do
          #_(prn 'on
                 (:note midi-message)
                 (:velocity midi-message)) ;; TODO: remove-me

          ;; (.noteOn channel
          ;;          (:note midi-message)
          ;;          (:velocity midi-message))
          (swap! pressed-keys conj (:note midi-message))
          (async/>!! @event-channel-atom {:type :xxx})
          )

        (= :note-off (:command midi-message))
        (do ;; (.noteOff channel
          ;;           (:note midi-message))
          (swap! pressed-keys disj (:note midi-message))
          (async/>!! @event-channel-atom {:type :xxx})
          )))

(defn best-fitting-root-index [scale notes]
  (->> (for [root-index (range 12)]
         {:root-index root-index
          :hit-ratio (/ (->> notes
                             (map :pitch)
                             (filter (partial in-key? scale root-index))
                             (count))
                        (count notes))})
       (sort-by :hit-ratio)
       (reverse)
       (first)
       (:root-index)))


(comment

  (do (println (.ntpValue (TimeStamp/getNtpTime (long (+ 100 (live/now))))))
      (println (.ntpValue (TimeStamp/getCurrentTime))))
  ;; => -1931497290938858865

  ;; => -1931497286403373401

  (.getNtpTime (TimeStamp/getCurrentTime)
               (long (live/now)))
  ;; => 1636264359068
  ;; => 1636264351156
  (live/now)
  ;; => 1636264362092;; => 1636264349229


  (midi/midi-devices)
  (do (reset! notes
              (->> (tracks #_"/Users/jukka/Downloads/tassako_taa_oli.mid"
                           #_"/Users/jukka/google-drive/jukka/music/Believer_-_Imagine_Dragons_Advanced.mid"
                           "/Users/jukka/google-drive/jukka/music/Hatsune_Miku_Ivean_Polkka.mid"
                           #_"/Users/jukka/google-drive/jukka/music/testi.mid")
                   (drop 0)
                   (first)
                   #_(map (partial quantisize-note
                                   (/ 1 8)))))
      nil)



  (note-index-to-name (best-fitting-root-index minor
                                               #_major
                                               (first (tracks "/Users/jukka/Downloads/tassako_taa_oli.mid"))))

  (def synth (midi/midi-out))
  (do (midi/midi-note-on synth 40 100)
      (Thread/sleep 500)
      (midi/midi-note-off synth 0))

  ) ;; TODO: remove-me
