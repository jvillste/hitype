(ns hitype.ship-war
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
            [flow-gl.graphics.rectangle :as rectangle]
            [flow-gl.swing.window :as swing-window]
            [flow-gl.csp :as csp]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.font :as font])
  (:import java.awt.geom.AffineTransform))


(defn load-image [file-name]
  (if-let [resource (io/resource file-name)]
    (buffered-image/create-from-file (.getPath resource))
    (throw (ex-info (str "Tiedostoa " file-name " ei löydy")
                    {}))))

(def heroes (load-image "hero-tiles/heart.png"))
(def dragons (load-image "hero-tiles/heart.png"))
(def wheat [(load-image "hero-tiles/heart.png")
            (load-image "hero-tiles/heart.png")
            (load-image "hero-tiles/heart.png")])
(def vino_tyyppi (load-image "hero-tiles/heart.png"))
(def heart (load-image "hero-tiles/heart.png"))

(defn clip-tile [atlas x y]
  (buffered-image/clip atlas
                       (* (- x 1)
                          32)
                       (* (- y 1)
                          32)
                       32 32))

#_(def hero (clip-tile heroes 4 5))
(def hero (load-image "hero-tiles/heart.png") #_vino_tyyppi #_(load-image "hero.png")
  #_(clip-tile heroes 4 4))
(def dragon (load-image "hero-tiles/heart.png") #_heart #_(clip-tile dragons 2 4))

(def load-image (memoize load-image))

(defn handle-event! [event state-atom]
  (cond (and (= :key-pressed (:type event))
             (nil? (:pressed-key @state-atom)))
        (swap! state-atom assoc
               :pressed-key (:key event)
               :key-pressed-time (System/currentTimeMillis))

        (= :key-released (:type event))
        (swap! state-atom assoc
               :pressed-key nil
               :key-pressed-time nil)))

(defn handle-keyboard-event [event state]
  (cond (and (= :key-pressed (:type event))
             (nil? (:pressed-key state)))
        (assoc state
               :pressed-key (:key event)
               :key-pressed-time (System/currentTimeMillis))

        (= :key-released (:type event))
        (assoc state
               :pressed-key nil
               :key-pressed-time nil)

        :else
        state))

(def grid-size 60)

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

(def next-id-atom (atom 0))

(defn new-id []
  (swap! next-id-atom inc))

(defn add-actor [state actor]
  (let [id (new-id)]
    (assoc-in state [:actors id] (assoc actor :id id))))

(defn remove-actor [state actor-id]
  (update state :actors dissoc actor-id))

(defn update-actor [game-state actor-id function & parameters]
  (apply update-in
         game-state
         [:actors actor-id]
         function
         parameters))

(defn constrain [minimum maximum value]
  (min (max minimum value)
       maximum))

(defn attack [game-state target-actor]
  (let [new-health (dec (:health target-actor))]
    (if (= 0 new-health)
      (remove-actor game-state
                    (:id target-actor))
      (update-actor game-state
                    (:id target-actor)
                    update :health dec))))

(defn actors-in-coordinates [game-state x y]
  (filter (fn [actor]
            (and (= x (:x actor))
                 (= y (:y actor))))
          (vals (:actors game-state))))


(defn- attack-or-move [game-state next-coordinates actor-id]
  (if-let [target-actor (first (actors-in-coordinates game-state
                                                      (:x next-coordinates)
                                                      (:y next-coordinates)))]
    (if (not (= actor-id
                (:id target-actor)))
      (attack game-state target-actor)
      game-state)
    (update-actor game-state
                  actor-id
                  merge next-coordinates)))


(defn update-wheat [wheat-state game-state]
  (let [time-now (System/currentTimeMillis)]
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
           :last-growth-time (System/currentTimeMillis)
           :image (get wheat 0)
           :update-function #'update-wheat}))

(defn update-state [game-state]
  (reduce (fn [game-state actor-id]
            (if (contains? (:actors game-state)
                           actor-id)
              ((get-in game-state [:actors actor-id :update-function])
               actor-id game-state)
              game-state))
          game-state
          (keys (:actors game-state))))

(defn start-update-loop [state-atom]
  (future (loop []
            (Thread/sleep 100)
            (swap! state-atom update-state)
            (recur))))


(defn actor-state [game-state actor-id]
  (get-in game-state [:actors actor-id]))

(defn act-with-throttle [game-state actor-id delay act-function]
  (let [time-now (System/currentTimeMillis) #_(System/currentTimeMillis)
        last-move-time (:last-move-time (actor-state game-state
                                                     actor-id))]
    (if (or (nil? last-move-time)
            (< delay
               (- time-now
                  last-move-time)))
      (update-actor (act-function game-state)
                    actor-id
                    assoc :last-move-time time-now)
      game-state)))

(defn update-dragon [actor-id game-state]
  (act-with-throttle game-state
                     actor-id
                     1500
                     (fn [game-state]
                       (attack-or-move game-state
                                       (update (select-keys (actor-state game-state
                                                                         actor-id)
                                                            [:x :y])
                                               :x
                                               #(constrain 0 15 ((if (< 0.5 (rand))
                                                                   inc dec)
                                                                 %)))
                                       actor-id))))

(defn update-player [actor-id game-state]
  (cond
    (:pressed-key game-state)
    (let [speed 10
          next-coordinates (let [{:keys [x y]} (actor-state game-state
                                                            actor-id)]
                             (case (:pressed-key game-state)
                               :right {:x (+ x speed)
                                       :y y}
                               :left {:x (- x speed)
                                      :y y}
                               :down {:x x
                                      :y (+ y speed)}
                               :up {:x x
                                    :y (- y speed)}
                               {:x x
                                :y y}))]

      (update-actor game-state
                    actor-id
                    merge
                    next-coordinates))

    :default
    game-state))

(defn initial-state []
  (reduce add-actor
          {:actors {}}
          (concat [{:x 0
                    :y 0
                    :health 10
                    :image #'hero
                    :update-function #'update-player}]
                  #_(take 30 (repeatedly (fn []
                                         {:x (rand-int 30)
                                          :y (rand-int 30)
                                          :health 3
                                          :image #'dragon
                                          :update-function #'update-dragon}))))))

(def font (font/create-by-name "CourierNewPSMT" 40))


(defn render-game-world [state graphics]
  ;; (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (rectangle/fill graphics
                  [255 255 255 255]
                  400
                  400
                  0
                  0)
  (let [transform (AffineTransform.)
        scale 1]

    (.setToTranslation transform 0 200)
    (.setTransform graphics transform)
    (text/draw graphics [0 0 0 255] font "moi2")

    (doseq [actor (vals (:actors state))]
      (.setToTranslation transform
                         (* (:x actor)
                            scale)
                         (* (:y actor)
                            scale))
      (.setTransform graphics transform)
      (buffered-image/draw-image graphics heart 100 100))))

(defn view [state-atom]
  (animation/swap-state! animation/set-wake-up 100)
  @animation/state-atom
  {:render (partial render-game-world state-atom)}
  #_(layouts/superimpose
   (for [actor (vals (:actors @state-atom))]
     (tile actor))))

(defn view-constructor []
  (let [state-atom (dependable-atom/atom (initial-state))]
    (keyboard/set-focused-event-handler! (fn [event]
                                           (handle-event! event state-atom)))
    (start-update-loop state-atom)

    (fn [] (view state-atom))))

(defn handle-events [state events]
  (reduce (fn [state event]
            (if (= :close-requested (:type event))
              (assoc state :close-requested true)
              (if (= :keyboard (:source event))
                (handle-keyboard-event event state)
                state)))
          state
          events))

(defn start []
  (prn "----------") ;; TODO: remove-me

  (.start (Thread. (fn []
                     (let [window (swing-window/create 400 400)]
                       (loop [state (initial-state)]
                         (Thread/sleep 1)
                         (let [state (update-state (handle-events state
                                                                  (csp/drain (:event-channel window)
                                                                             1)))
                               buffer-strategy (.getBufferStrategy (:canvas window))
                               graphics (.getDrawGraphics buffer-strategy)]
                           (try
                             (render-game-world state graphics)
                             (finally
                               (.dispose graphics)
                               (.show buffer-strategy)))
                           (when (not (:close-requested state))
                             (recur state))))
                       (println "exitted game loop")))))

  #_(application/start-window view-constructor))



(comment
  (application/start-window (fn []
                              (visuals/text "haa" {:color [0 0 0 255]})))


  (with-bindings (application/create-event-handling-state)
    (layout/do-layout-for-size (view-compiler/compile (layouts/superimpose (tile 1 1 hero 2)))
                               500 500 ))

  (repeat 10
        (str "Moi " (read-line)))
  ) ;; TODO: remove-me
