(ns hitype.stave-grinder
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [fungl.component.text-area :as text-area]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.util :as util]
            [hitype.util :as hitype-util]
            [flow-gl.gui.keyboard :as keyboard]
            [fungl.cache :as cache]
            [clojure.test :refer :all]
            [clojure.set :as set]
            [overtone.midi.file :as midi-file]
            [clojure.xml :as xml]
            [medley.core :as medley]
            [overtone.midi :as midi]
            [clojure.core.async :as async]

            ;; [overtone.live      :as live]
            ;; [overtone.inst.piano :as piano]
            ;; [overtone.music.time :as time]
            ))

(defn launchpad-device []
  (medley/find-first (fn [device]
                       (= "Launchpad X LPX MIDI Out"
                          (:description device)))
                     (midi/midi-sources)))

(defonce pressed-keys (dependable-atom/atom #{}))
(comment
  (reset! pressed-keys #{})
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

(def sexteenth-note-width 15)
(def line-width 2)
(def line-gap (/ sexteenth-note-width 1.4))
(def row-height (* 5 line-gap))
(def middle-finger-shade 235)
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
  (contains? (set (notes-in-scale 0 12 scale))
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

(defn scale-position [pitch]
  (mod pitch 12))


(defn line [pitch]
  (layouts/superimpose (let [number-in-row (mod pitch 5)
                             fat? (contains? ;;  #{ 1 2 3 4 }
                                    #{ 1  3 4 }
                                   number-in-row)
                             shaded-background? (in-scale? major pitch)
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
                          (when shaded-background?
                            (assoc (visuals/rectangle-2 :fill-color [middle-finger-shade
                                                                     middle-finger-shade
                                                                     middle-finger-shade
                                                                     255])
                                   :height line-gap))))))

(defn row []
  (layouts/superimpose (assoc (visuals/rectangle-2 :fill-color [255 255 255 0])
                              :height row-height
                              :width (* 16 sexteenth-note-width))
                       (for [x (range 0 5)]
                         (let [fat? (contains? #{4}
                                               x)
                               shaded-background? (contains? #{1}
                                                             #_#{0 2}
                                                             x)
                               line-width (if fat?
                                            (* 2 line-width)
                                            line-width)]
                           (layouts/superimpose
                            (assoc (visuals/rectangle-2 :fill-color (if fat?
                                                                      [0 0 0 255]
                                                                      [gray-line-shade
                                                                       gray-line-shade
                                                                       gray-line-shade
                                                                       255]))
                                   :y (- (* x line-gap)
                                         (/ line-width 2))
                                   :width (* sexteenth-note-width 16)
                                   :height line-width)
                            (when shaded-background?
                              (assoc (visuals/rectangle-2 :fill-color [middle-finger-shade
                                                                       middle-finger-shade
                                                                       middle-finger-shade
                                                                       255])
                                     :y (* x line-gap)
                                     :width (* sexteenth-note-width 16)
                                     :height line-gap)))))

                       #_(assoc (visuals/rectangle-2 :fill-color [0 0 0 255])
                                :width (* sexteenth-note-width 16)
                                :height (* 2 line-width)
                                :y (- (* 4 line-gap)
                                      line-width))))

(def middle-c 60)

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
                                            [100 100 100 255]
                                            [0 0 0 255]))
         :x (* (:start note)
               (* 4 sexteenth-note-width))
         :y (- (* (- (:pitch note)
                     middle-c)
                  line-gap))
         :width (* (- (:end note)
                      (:start note))
                   (* 4 sexteenth-note-width))
         :height line-gap))




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
                                               (scale-position pitch))
                                          (if (= 0 (scale-position pitch))
                                            (octave-number pitch)))
                                     (* line-gap 0.9))
                               (assoc :y
                                      (+ 1
                                         (* line-gap
                                            (- maximum-pitch
                                               pitch)))))))))

(comment
  (quot 10 5)
  ) ;; TODO: remove-me


(defn stave [notes]
  (let [minimum-pitch (- middle-c (* 2 12)) #_(apply min (map :pitch notes))
        maximum-pitch (+ middle-c (* 2 12)) #_(apply max (map :pitch notes))
        first-note-start-time (:start (first notes))
        measure-count (/ (- (apply max (map :end notes))
                            first-note-start-time)
                         4)]
    (layouts/superimpose (for [measure-number (range measure-count)]
                           (assoc (measure minimum-pitch
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
                                  :width (* measure-count sexteenth-note-width 16)
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

(defonce notes (dependable-atom/atom []))

(defn wrap-keyboard-event-hanlder [handler]
  (fn [event]
    (when (= :key-released (:type event))
      (handler {:näppäin (:key event)
                :merkki (str (:character event))}))))

(defmacro piirrä-2 [& {:keys [tila alkuarvo näppäimistökäsittelijä kuva aloitus]}]
  `(do (reset! view-atom
               (let [~tila (dependable-atom/atom ~alkuarvo)]
                 (fn []
                   (when ~aloitus (~aloitus))

                   (fn []
                     (keyboard/set-focused-event-handler! (wrap-keyboard-event-hanlder ~näppäimistökäsittelijä))
                     ~kuva))))
       nil))


(defn handle-keyboard-event2 [state-atom event]
  (when (= :key-pressed (:type event))
    (cond (= :right (:key event))
          (swap! state-atom update :start inc)

          (= :left (:key event))
          (swap! state-atom update :start dec))))

(defn handle-keyboard-event [state-atom event]
  (#'handle-keyboard-event2 state-atom event))

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

(defn playing-notes [notes-by-bar time]
  (filter (fn [note]
            (and (<= (:start note)
                     time)
                 (>= (:end note)
                     time)))
          (get notes-by-bar
               (int (/ time 4)))))

(defn midi-change-events [previous-playing-notes-set current-playing-notes-set]
  (concat (for [stopped-note (set/difference previous-playing-notes-set current-playing-notes-set)]
            {:pitch (:pitch stopped-note)
             :command :note-off})
          (for [started-note (set/difference current-playing-notes-set
                                             previous-playing-notes-set)]
            {:pitch (:pitch started-note)
             :command :note-on})))

(defn play-notes [beats-per-minute start-time notes]
  (let [notes-by-bar (notes-by-bar notes)
        last-bar (apply max (keys notes-by-bar))
        stop-atom (atom false)
        playing-notes-atom (atom #{})
        play-start-time (System/currentTimeMillis)
        beat-time-now (fn []
                        (* (/ (- play-start-time
                                 (System/currentTimeMillis))
                              1000 60)
                           beats-per-minute))]
    (prn 'starting (beat-time-now)) ;; TODO: remove-me
    (.start (Thread. (fn []
                       (while (and (not @stop-atom)
                                   (< last-bar
                                      (inc (Math/ceil (/ (beat-time-now)
                                                         4)))))
                         (prn 'beat-time-now (beat-time-now))
                         (let [playing-notes-set (set (playing-notes notes-by-bar
                                                                     (beat-time-now)))]
                           (prn playing-notes-set) ;; TODO: remove-me

                           (doseq [midi-event (midi-change-events @playing-notes-atom
                                                                  playing-notes-set)]
                             (prn midi-event))
                           (reset! playing-notes-atom playing-notes-set))
                         (Thread/sleep 1000)))))
    stop-atom))

(comment
  (midi/midi-sinks)
  (midi/midi-note-off)
  (do (.start (Thread. (fn []
                       (Thread/sleep 100)
                       (prn "moi"))))
      (prn 'started))

  (let [stop-atom (play-notes 120 0 [{:start 1
                                      :end 2}])]
    (Thread/sleep 3000)
    (reset! stop-atom true))


  ) ;; TODO: remove-me


(defn base-view []
  (let [state-atom (dependable-atom/atom {:start 0})]
    (keyboard/set-focused-event-handler! (partial handle-keyboard-event
                                                  state-atom))
    (fn []
      (animation/swap-state! animation/set-wake-up 1000)
      @animation/state-atom
      (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                           (layouts/center
                            (layouts/vertically-2
                             {:margin 10}
                             (stave (->> @notes
                                         (drop-while (fn [note]
                                                       (< (:start note)
                                                          (* 4 (:start @state-atom)))))
                                         (take-while (fn [note]
                                                       (< (:start note)
                                                          (* 4 (+ (:start @state-atom)
                                                                  6))))))
                                    #_(for [n (range 8)]
                                        (let [note (note-in-scale 0 major n)]
                                          {:pitch note
                                           :number-in-scale (inc (mod n 7))
                                           :start (* 2 n)
                                           :duration 2})))
                             #_(text (pr-str @pressed-keys))))))))

(declare event-channel)

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (def event-channel (application/start-window #'base-view)))

(defn handle-midi-message [midi-message]
  (cond (= :note-on (:command midi-message))
        (do
          (swap! pressed-keys conj (:note midi-message))
          (async/>!! event-channel {:type :repaint}))

        (= :note-off (:command midi-message))
        (do (swap! pressed-keys disj (:note midi-message))
            (async/>!! event-channel {:type :repaint}))))

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

  (midi/midi-handle-events (midi/midi-in "Launchpad X LPX MIDI Out")
                           #'handle-midi-message)

  (do (reset! notes
              (->> (tracks "/Users/jukka/Downloads/tassako_taa_oli.mid"
                           #_"/Users/jukka/Downloads/Believer_-_Imagine_Dragons_Advanced.mid"
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
