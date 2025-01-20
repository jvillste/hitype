(ns hitype.clicker-simulator
  (:require [flow-gl.gui.visuals :as visuals]
            [medley.core :as medley]
            [fungl.application :as application]
            [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [flow-gl.gui.animation :as animation]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [fungl.dependable-atom :as dependable-atom]
            [flow-gl.gui.keyboard :as keyboard]
            [fungl.view-compiler :as view-compiler]
            [clojure.core.async :as async]))

(defn load-image [file-name]
  (if-let [resource (io/resource file-name)]
    (buffered-image/create-from-file (.getPath resource))
    (throw (ex-info (str "Tiedostoa " file-name " ei löydy")
                    {}))))

(def pick-a-pet-image (load-image "clicker-simulator/pick-a-pet.jpeg"))

(def pets [{:id :cat
            :coins 1
            :image (load-image "clicker-simulator/cat.jpeg")}
           {:id :dog
            :coins 1
            :image (load-image "clicker-simulator/dog.jpeg")}
           {:id :axolotl
            :coins 1
            :image (load-image "clicker-simulator/axolotl.jpeg")}])

(defonce event-channel-atom (atom nil))


(defn scale-to-width [width image]
  {:node image
   :width width
   :height (* (.getHeight (:buffered-image image))
              (/ width
                 (.getWidth (:buffered-image image))))})

(defn master-page [content]
  (layouts/superimpose (visuals/rectangle-2 :fill-color [155 155 155 255])
                       (layouts/center-horizontally
                        (layouts/with-margin 200
                          content))))

(defn pick-a-pet [_on-pet-chosen]
  (let [state-atom (dependable-atom/atom {})]
    (fn [on-pet-chosen]
      (master-page (layouts/vertically-2 {:margin 10
                                          :centered? true}
                                         (scale-to-width 500
                                                         (visuals/image pick-a-pet-image))
                                         (visuals/text-area (str "pet: " (:id (:chosen-pet @state-atom))))
                                         (layouts/horizontally-2 {:margin 100}
                                                                 (for [pet pets]
                                                                   (layouts/box 30
                                                                                (visuals/rectangle-2 :fill-color (if (= (:id pet)
                                                                                                                        (:id (:chosen-pet @state-atom)))
                                                                                                                   [0 0 255 255]
                                                                                                                   [255 255 255 255])
                                                                                                     :corner-arc-radius 100)
                                                                                {:node (scale-to-width 400 (visuals/image (:image pet)))
                                                                                 :mouse-event-handler (fn [_node event]
                                                                                                        (when (= :mouse-clicked (:type event))
                                                                                                          (if (= (:id pet)
                                                                                                                 (:id (:chosen-pet @state-atom)))
                                                                                                            (swap! state-atom assoc :chosen-pet nil)
                                                                                                            (swap! state-atom assoc :chosen-pet pet)))
                                                                                                        event)})))

                                         {:node (layouts/box 100
                                                             (visuals/rectangle-2 :fill-color (if (:chosen-pet @state-atom)
                                                                                                [0 155 0 255]
                                                                                                [200 200 200 255])
                                                                                  :corner-arc-radius 100)
                                                             (visuals/text "OK"))
                                          :mouse-event-handler (fn [_node event]
                                                                 (when (= :mouse-clicked (:type event))
                                                                   (on-pet-chosen (:chosen-pet @state-atom)))
                                                                 event)})))))

(defn button [label color on-click!]
  {:node (layouts/box 100
                      (visuals/rectangle-2 :fill-color color
                                           :corner-arc-radius 100)
                      (visuals/text-area label))
   :mouse-event-handler (fn [_node event]
                          (when (= :mouse-clicked (:type event))
                            (on-click!))
                          event)})

(defn clicker-room [on-click]
  ;; (layouts/superimpose (visuals/rectangle-2 :fill-color [155 155 155 255])
  ;;                      (layouts/center-horizontally
  ;;                       (layouts/with-margin 200
  ;;                         content)))
  (button "Click me!"
          [0 155 0 255]
          on-click))

(defn menu [state-atom]
  (layouts/vertically-2 {:margin 10
                         :centered? true}
                        (button (str "Coins: " (:coins @state-atom))
                                [191, 188, 0 255]
                                (fn []))
                        (button (str "Gems: " 0)
                                [232, 3, 252 255]
                                (fn []))
                        (button (str "Rebirths: " 0)
                                [0 0 255 255]
                                (fn []))))

(def roll-rng (load-image "roll-RNG/menu.png"))
(defn main []
  #_(apply layouts/vertically
           (interpose {:height 30}
                      (repeat 4 (visuals/text-area "hello"))))

  #_(layouts/vertically (interpose {:height 30}
                                 (repeat 2 (visuals/text-area "hello")))
                      {:height 100}
                      (interpose {:height 30}
                                 (repeat 2 (visuals/text-area "world"))))


  (visuals/image roll-rng)
  #_(let [state-atom (dependable-atom/atom {:state #_:clicker-room :pick-a-pet
                                            :coins 0
                                            :chosen-pet (medley/find-first #(= :dog (:id %))
                                                                           pets)})]
      (fn []
        (layouts/grid [[(menu state-atom)
                        (cond (= :pick-a-pet (:state @state-atom))
                              [pick-a-pet (fn [chosen-pet]
                                            (swap! state-atom assoc
                                                   :chosen-pet chosen-pet
                                                   :state :clicker-room))]

                              (= :clicker-room (:state @state-atom))
                              [clicker-room
                               (fn []
                                 (swap! state-atom update :coins (fn [coins]
                                                                   (+ coins
                                                                      (:coins (:chosen-pet @state-atom))))))])]]))))

(defn start []
  (prn "----------") ;; TODO: remove-me
  (reset! event-channel-atom
          (application/start-application #'main
                                         :on-exit #(reset! event-channel-atom nil))))

(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))
