(ns hitype.stave-grinder
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [flow-gl.swing.window :as window]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [fungl.component.text-area :as text-area]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.util :as util]
            [hitype.util :as hitype-util]
            [flow-gl.gui.keyboard :as keyboard]
            [fungl.cache :as cache]
            [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.xml :as xml]
            [medley.core :as medley]
            [clojure.core.async :as async])
  (:import java.util.concurrent.PriorityBlockingQueue
           [org.apache.commons.net.ntp TimeStamp]))

(defn load-image [file-name]
  (if-let [resource (io/resource file-name)]
    (buffered-image/create-from-file (.getPath resource))
    (throw (ex-info (str "Tiedostoa " file-name " ei löydy")
                    {}))))

(defonce image (buffered-image/create-from-file "/Users/jukka/google-drive/jukka/gfx/Made_AmigaGFX_DemosceneArchive/Made - Sun Flower (28-08-1997).png"))

(def cursor-image (buffered-image/create 16 16))

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



(defn view []
  (layouts/superimpose (assoc (visuals/rectangle-2 :fill-color [0 0 200 55])
                              :x 10
                              :y 10
                              :width 20
                              :height 20)))

(defn handle-keyboard-event2 [state-atom event]
  #_(prn event)

  (case (:type event)
    :key-pressed
    (do (case (:key event)
          :1 (swap! state-atom assoc :scale 5)
          :2 (swap! state-atom assoc :scale 20)
          nil)

        (swap! state-atom update :keys-down conj (:key event)))
    :key-released
    (swap! state-atom update :keys-down disj (:key event))

    nil))

(defn handle-keyboard-event [state-atom event]
  (#'handle-keyboard-event2 state-atom event))

(defn add-vectors [vector-a vector-b]
  {:x (+ (:x vector-a)
         (:x vector-b))
   :y (+ (:y vector-a)
         (:y vector-b))})

(defn substract-vectors [vector-a vector-b]
  {:x (- (:x vector-a)
         (:x vector-b))
   :y (- (:y vector-a)
         (:y vector-b))})

(defn handle-mouse-event [state-atom _node event]
  (when (:x event)
    (let [state @state-atom]
      (swap! state-atom
             assoc
             :pointer {:x (:x event)
                       :y (:y event)})
      (case (:type event)
        :mouse-pressed (swap! state-atom
                              assoc
                              :drag-start {:x (:x event)
                                           :y (:y event)})
        :mouse-released (do (when (contains? (:keys-down @state-atom)
                                             :space)
                              (swap! state-atom
                                     assoc
                                     :canvas-offset
                                     (add-vectors (:canvas-offset @state-atom)
                                                  (:canvas-drag-offset @state-atom))
                                     :canvas-drag-offset {:x 0 :y 0}))
                            (swap! state-atom
                                   dissoc
                                   :drag-start))

        :mouse-moved (when (and (contains? (:keys-down @state-atom)
                                           :space)
                                (:drag-start state))
                       (swap! state-atom
                              assoc
                              :canvas-drag-offset
                              (substract-vectors event
                                                 (:drag-start state))))
        nil)))
  #_(prn (dissoc event :nodes-under-mouse))
  event)

(defn quantisize [scale value]
  (* scale
     (int (/ value
             scale))))

(deftest test-quantisize
  (is (= 5
         (quantisize 5 6)))
  (is (= 5
         (quantisize 5 9))))

(defn canvas-view [state-atom]
  (let [state @state-atom]
    (-> (layouts/superimpose (let [canvas-offset (add-vectors (:canvas-offset state)
                                                              (:canvas-drag-offset state))]
                               (assoc (layouts/scale (:scale state)
                                                     (:scale state)
                                                     (visuals/image image))
                                      :x (:x canvas-offset)
                                      :y (:y canvas-offset)))

                             (assoc (visuals/rectangle-2 :fill-color (:color state))
                                    :x (quantisize (:scale state)
                                                   (-> state :pointer :x))
                                    :y (quantisize (:scale state)
                                                   (-> state :pointer :y))
                                    :width (:scale state)
                                    :height (:scale state))
                             (assoc (visuals/rectangle-2 :fill-color [255 255 255 255])
                                    :x (-> state :pointer :x)
                                    :y (-> state :pointer :y)
                                    :width (/ (:scale state) 2)
                                    :height (/ (:scale state) 2))
                             (text (pr-str state) 20 [2 255 255 255]))
        (assoc :mouse-event-handler [handle-mouse-event state-atom]))))

(defn base-view []
  (let [state-atom (dependable-atom/atom {:scale 5
                                          :keys-down #{}
                                          :pointer {:x 0 :y 0}
                                          :canvas-offset {:x 0 :y 0}
                                          :canvas-drag-offset {:x 0 :y 0}
                                          :color [100 100 0 255]})]
    (keyboard/set-focused-event-handler! (partial handle-keyboard-event
                                                  state-atom))
    (fn []
      (animation/swap-state! animation/set-wake-up 500)
      @animation/state-atom
      (canvas-view state-atom))))

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (def window (window/create 1500 1200))
  (window/set-cursor window (window/create-cursor cursor-image))
  (application/start-window #'base-view
                            :window window))
