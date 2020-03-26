(ns hitype.hero
  (:require [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [flow-gl.gui.animation :as animation]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [fungl.dependable-atom :as dependable-atom]
            [flow-gl.gui.keyboard :as keyboard]))

(defn load-image [file-name]
  (if-let [resource (io/resource file-name)]
    (buffered-image/create-from-file (.getPath resource))
    (throw (ex-info (str "Tiedostoa " file-name " ei löydy")
                    {}))))

(def heroes (load-image "dg_classm32.gif"))
(def dragons (load-image "dg_dragon32.gif"))

(defn clip-sprite [atlas x y]
  (buffered-image/clip atlas
                       (* (- x 1)
                          32)
                       (* (- y 1)
                          32)
                       32 32))

#_(def hero (clip-sprite heroes 4 5))
(def hero (load-image "hero.png"))
(def dragon (clip-sprite dragons 2 4))

(def load-image (memoize load-image))

(defn handle-event! [event state-atom]
  (cond (and (= :key-pressed (:type event))
             (nil? (:pressed-key @state-atom)))
        (swap! state-atom assoc
               :pressed-key (:key event)
               :key-pressed-time (animation/time!))

        (= :key-released (:type event))
        (swap! state-atom assoc
               :pressed-key nil
               :key-pressed-time nil)))

(def grid-size 100)

(defn sprite [x y image]
  (assoc (visuals/image image)
         :x (* grid-size x)
         :y (* grid-size y)
         :width grid-size
         :height grid-size))

;; state

(defn constrain [minimum maximum value]
  (min (max minimum value)
       maximum))

(defn update-dragon [dragon-state game-state]
  (let [time-now (animation/time!)]
    (if (or (nil? (:last-move-time dragon-state))
            (< 500
               (- time-now
                  (:last-move-time dragon-state))))
      (let [dragon-state (assoc dragon-state :last-move-time time-now)]
        (if (< 0.5 (rand))
          (update dragon-state :x (fn [x] (constrain 0 15 (inc x))))
          (update dragon-state :x (fn [x] (constrain 0 15 (dec x))))))
      dragon-state)))

(defn update-state [state]
  (update state :actors (fn [actors]
                        (for [actor actors]
                          ((:update-function actor) actor state)))))

(defn start-update-loop [state-atom]
  (future (loop []
            (Thread/sleep 50)
            (swap! state-atom update-state)
            (recur))))

(defn update-player [player-state game-state]
  (let [time-now (animation/time!)]
    (if (and (:pressed-key game-state)
             (or (nil? (:last-move-time player-state))
                 (< 200
                    (- time-now
                       (:last-move-time player-state)))))
      (let [player-state (assoc player-state :last-move-time time-now)]
        (case (:pressed-key game-state)
          :right (update player-state :x inc)
          :left (update player-state :x dec)
          :down (update player-state :y inc)
          :up (update player-state :y dec)
          player-state))
      player-state)))

(defn initial-state []
  {:actors (concat [{:x 0
                     :y 0
                     :image #'hero
                     :update-function #'update-player}]
                   (take 30 (repeatedly (fn []
                                          {:x (rand-int 30)
                                           :y (rand-int 30)
                                           :image #'dragon
                                           :update-function #'update-dragon}))))})

(defn view [state-atom]
  (animation/swap-state! animation/set-wake-up 100)
  @animation/state-atom

  (apply layouts/superimpose
         (for [actor (:actors @state-atom)]
           (sprite (:x actor)
                   (:y actor)
                   @(:image actor)))))

(defn view-constructor []
  (let [state-atom (dependable-atom/atom (initial-state))]
    (keyboard/set-focused-event-handler! (fn [event]
                                           (handle-event! event state-atom)))
    (start-update-loop state-atom)

    (fn [] (view state-atom))))

(defn start []
  (application/start-window view-constructor))
