(ns hitype.hero
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
            [fungl.view-compiler :as view-compiler]))

(defn load-image [file-name]
  (if-let [resource (io/resource file-name)]
    (buffered-image/create-from-file (.getPath resource))
    (throw (ex-info (str "Tiedostoa " file-name " ei löydy")
                    {}))))

(def heroes (load-image "dg_classm32.gif"))
(def dragons (load-image "dg_dragon32.gif"))
(def wheat [(load-image "hero-tiles/wheat_1.png")
            (load-image "hero-tiles/wheat_2.png")
            (load-image "hero-tiles/wheat_3.png")])
(def vino_tyyppi (load-image "hero-tiles/vino_tyyppi.png"))
(def heart (load-image "hero-tiles/heart.png"))

(defn clip-tile [atlas x y]
  (buffered-image/clip atlas
                       (* (- x 1)
                          32)
                       (* (- y 1)
                          32)
                       32 32))

#_(def hero (clip-tile heroes 4 5))
(def hero #_vino_tyyppi #_(load-image "hero.png")
  (clip-tile heroes 4 4))
(def dragon #_heart (clip-tile dragons 2 4))

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

(def grid-size 160)

(defn tile [{:keys [x y image health]}]
  (assoc (layouts/vertically-2 {:margin 3}
                               (assoc (visuals/image (if (var? image)
                                                       @image
                                                       image))
                                      :width grid-size
                                      :height grid-size)
                               (layouts/horizontally-2 {:margin 1}
                                                       (take health
                                                             (repeat (assoc (visuals/image heart)
                                                                            :width (/ grid-size 10)
                                                                            :height (/ grid-size 10))))))
         :x (* grid-size x)
         :y (* grid-size y)))

;; state

(defn update-actor [game-state actor-id function & parameters]
  (apply update-in
         game-state
         [:actors actor-id]
         function
         parameters))

(defn constrain [minimum maximum value]
  (min (max minimum value)
       maximum))

(defn update-dragon [actor-id game-state]
  (update-actor game-state
                actor-id
                (fn [dragon-state]
                  (let [time-now (animation/time!)]
                    (if (or (nil? (:last-move-time dragon-state))
                            (< 1500
                               (- time-now
                                  (:last-move-time dragon-state))))
                      (let [dragon-state (assoc dragon-state :last-move-time time-now)]
                        (if (< 0.5 (rand))
                          (update dragon-state :x (fn [x] (constrain 0 15 (inc x))))
                          (update dragon-state :x (fn [x] (constrain 0 15 (dec x))))))
                      dragon-state)))))

(defn update-wheat [wheat-state game-state]
  (let [time-now (animation/time!)]
    (if (and (< 1000
                (- time-now
                   (:last-growth-time wheat-state)))
             (> 3 (:size wheat-state)))
      (-> wheat-state
          (update :size inc)
          (assoc :last-growth-time time-now))
      wheat-state)))

(defn add-wheat [game-state x y]
  (update game-state :actors conj
          {:x x
           :y y
           :size 0
           :last-growth-time (animation/time!)
           :image (get wheat 0)
           :update-function #'update-wheat}))

(defn update-state [game-state]
  (reduce (fn [game-state actor-id]
            ((get-in game-state [:actors actor-id :update-function])
             actor-id game-state))
          game-state
          (keys (:actors game-state))))

(defn start-update-loop [state-atom]
  (future (loop []
            (Thread/sleep 50)
            (swap! state-atom update-state)
            (recur))))

(defn actors-in-coordinates [game-state x y]
  (filter (fn [actor]
            (and (= x (:x actor))
                 (= y (:y actor))))
          (vals (:actors game-state))))

(defn actor-state [game-state actor-id]
  (get-in game-state [:actors actor-id]))

(defn update-player [player-actor-id game-state]
  (let [time-now (animation/time!)]
    (cond
      (and (:pressed-key game-state)
           (let [last-move-time (:last-move-time (actor-state game-state
                                                              player-actor-id))]
             (or (nil? last-move-time)
                 (< 200
                    (- time-now
                       last-move-time)))))
      (let [game-state (update-actor game-state
                                     player-actor-id
                                     assoc :last-move-time time-now)
            next-coordinates (let [{:keys [x y]} (actor-state game-state
                                                              player-actor-id)]
                               (case (:pressed-key game-state)
                                 :right {:x (inc x)
                                         :y y}
                                 :left {:x (dec x)
                                        :y y}
                                 :down {:x x
                                        :y (inc y)}
                                 :up {:x x
                                      :y (dec y)}
                                 {:x x
                                  :y y}))]

        (if-let [target-actor (first (actors-in-coordinates game-state
                                                            (:x next-coordinates)
                                                            (:y next-coordinates)))]
          (do (println "attack!" target-actor)
              (update-actor game-state
                            (:id target-actor)
                            update :health dec))
          (update-actor game-state
                        player-actor-id
                        merge next-coordinates)))

      :default
      game-state)))

(def next-id-atom (atom 0))

(defn new-id []
  (swap! next-id-atom inc))

(defn add-actor [state actor]
  (let [id (new-id)]
   (assoc-in state [:actors id] (assoc actor :id id))))

(defn initial-state []
  (reduce add-actor
          {:actors {}}
          (concat [{:x 0
                    :y 0
                    :health 10
                    :image #'hero
                    :update-function #'update-player}]
                  (take 30 (repeatedly (fn []
                                         {:x (rand-int 30)
                                          :y (rand-int 30)
                                          :health 3
                                          :image #'dragon
                                          :update-function #'update-dragon}))))))

(defn view [state-atom]
  (animation/swap-state! animation/set-wake-up 100)
  @animation/state-atom
  (layouts/superimpose
   (for [actor (vals (:actors @state-atom))]
     (tile actor))))

(defn view-constructor []
  (let [state-atom (dependable-atom/atom (initial-state))]
    (keyboard/set-focused-event-handler! (fn [event]
                                           (handle-event! event state-atom)))
    (start-update-loop state-atom)

    (fn [] (view state-atom))))

(defn start []
  (prn "----------") ;; TODO: remove-me

  (application/start-window view-constructor))


(comment
  (application/start-window (fn []
                              (visuals/text "haa" {:color [0 0 0 255]})))


  (with-bindings (application/create-event-handling-state)
    (layout/do-layout-for-size (view-compiler/compile (layouts/superimpose (tile 1 1 hero 2)))
                               500 500 ))
  ) ;; TODO: remove-me
