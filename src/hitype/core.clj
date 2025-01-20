(ns hitype.core
  (:require [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.font :as font]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.atom-registry :as atom-registry]
            [fungl.callable :as callable]
            [fungl.component.text-area :as text-area]
            [fungl.layouts :as layouts]
            [clojure.test :refer :all]
;;            [flow-gl.opengl.jogl.window :as jogl-window]
            [clojure.test :refer [deftest]]
            #_[overtone.live :as live]
            [fungl.dependable-atom :as debendable-atom]
            [flow-gl.gui.keyboard :as keyboard])
  (:import java.util.UUID))

(def original-frames nil #_(buffered-image/gif-frames (io/resource "explosion.gif")))
(def frames (vec (concat (drop 7 original-frames)
                         (take 7 original-frames))))
(comment

  (live/recording-start "resources/valo.wav")
  (live/recording-stop)
  ((live/sample "resources/valo.wav"))

  (live/demo 1.5
             (* (live/sin-osc 440)
                (live/sin-osc 229 0.3)
                (live/sin-osc 129 0.3)
                (live/sin-osc 129 0.3)))

  (live/stop-all)

  (def answer (live/sample "/Users/jukka/google-drive/src/hitype/resources/answer.wav"))

  (answer)

  (live/active-synths))

#_(def magnifying-glass (buffered-image/create-from-file (io/resource "magnifying-glass.png")))


(def font (font/create "LiberationSans-Regular.ttf" #_20 38))
(def symbol-font (font/create "LiberationSans-Regular.ttf" 20 #_58))

(defn text [string]
  (text-area/text (str string)
                  [0 0 0 255]
                  font))

(defn button-mouse-event-handler [handler node event]
  (when (= :mouse-clicked
           (:type event))
    (callable/call handler))
  event)

(defn button [message handler]
  (-> (layouts/box 10
                   (visuals/rectangle [200 200 200 255] 30 30)
                   (text message))
      (assoc :mouse-event-handler [button-mouse-event-handler handler])))


(defn bare-text-editor [id text handle-text-change]
  (text-area/text-area-3 id
                         :style {:color [0 0 0 255]
                                 :font  font}
                         :text text
                         :on-text-change handle-text-change))

(defn text-editor [id text handle-text-change]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color [255 255 255 255]
                                    :draw-color [200 200 200 255]
                                    :line-width 4
                                    :corner-arc-radius 30)
               (layouts/with-minimum-size 300 nil
                 (bare-text-editor id text handle-text-change))))

(defn character-animation [animation-key character]
  (let [phase (animation/phase! animation-key
                                1000)]
    (if (= 1 phase)
      (text (str character))
      (visuals/image (get frames
                          (int (animation/linear-mapping (animation/ping-pong 2
                                                                              phase)
                                                         0
                                                         (dec (count frames)))))))))
(defn view []
  (let [state-atom (debendable-atom/atom {:string "hello"
                                          :typed-character-count 0})]
    (fn []
      (animation/swap-state! animation/set-wake-up 2000)
      (keyboard/set-focused-event-handler! (fn [event]
                                             (let [state @state-atom]
                                               (when (and (= :key-pressed (:type event))
                                                          (= (nth (:string state)
                                                                  (:typed-character-count state))
                                                             (:character event))
                                                          (< (:typed-character-count state)
                                                             (count (:string state))))
                                                 (animation/swap-state! animation/start (:typed-character-count state))
                                                 (swap! state-atom update :typed-character-count inc)))))
      (let [state @state-atom]
        (-> (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                                 (layouts/with-margins 20 20 20 20
                                   (apply layouts/horizontally-2 {:margin 30}
                                          #_(text (pr-str state))
                                          (for [character-index (range (:typed-character-count state))]
                                            (character-animation character-index
                                                                 (nth (:string state)
                                                                      character-index)))))))))))

(defn start []
  (application/start-application #'view)
  #_(application/start-window (hitype)
                            :window
                            (jogl-window/create 600 1000
                                                :close-automatically true)))
