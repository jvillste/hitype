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
            [clojure.core.async :as async]))

(defn launchpad-device []
  (medley/find-first (fn [device]
                       (= "Launchpad X LPX MIDI Out"
                          (:description device)))
                     (midi/midi-sources)))

(defonce pressed-keys (dependable-atom/atom #{}))





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

(def sexteenth-note-width 10)
(def line-width 1)
(def line-gap (/ sexteenth-note-width 1.2))
(def row-height (* 5 line-gap))
(def middle-finger-shade 240)
(def gray-line-shade 200)
(def fat-line-shade 100)
(def note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])
(def major [2 2 1 2 2 2 1])

(defn note-in-scale [root scale n]
  (+ root
     (->> scale
          repeat
          (apply concat)
          (take n)
          (reduce +))))

(defn notes-in-scale [root count scale]
  (map (partial note-in-scale root scale)
       (range count)))

(deftest test-notes-in-scale
  (is (= '(0 2 3 5 6)
         (notes-in-scale 0 5 [2 1]))))

(defn in-scale? [scale pitch]
  (contains? (set (notes-in-scale 0 12 scale))
             (mod pitch 12)))

(defn octave-number [pitch]
  (dec (quot pitch 12)))

(defn scale-position [pitch]
  (mod pitch 12))


(defn line [pitch]
  (layouts/superimpose (let [number-in-row (mod pitch 5)
                             fat? (contains? #{1 2 4}
                                             number-in-row)
                             shaded-background? (in-scale? major pitch)
                             #_(contains? #{1}
                                          #_#{0 2}
                                          number-in-row)
                             line-width (if fat?
                                          (* 2 line-width)
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
  (let [minimum-pitch (- middle-c 12) #_(apply min (map :pitch notes))
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

(comment
  (reset! notes
          (->> (tracks "/Users/jukka/Downloads/Believer_-_Imagine_Dragons_Advanced.mid"
                       #_"/Users/jukka/google-drive/jukka/music/testi.mid")
               (drop 0)
               (first)
               #_(map (partial quantisize-note
                               (/ 1 8)))
               #_(take-while (fn [note]
                             (< (:start note)
                                (* 6 4))))))
  ) ;; TODO: remove-me


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
                                                                  8))))))
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

(comment

  (midi/midi-handle-events (midi/midi-in "Launchpad X LPX MIDI Out")
                           #'handle-midi-message)

  ) ;; TODO: remove-me
