(ns hitype.times-table
  (:require
   [clojure.test :refer :all]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.layouts :as layouts]
   [flow-gl.graphics.font :as font]
   [clojure.core.async :as async]))

(def maximum-exercise-points 3)

(def exercises (->> (for [x (range 2 10)
                          y (range 2 10)]
                      {:x x :y y})
                    (filter #(and (some  #{4} [(:x %) (:y %)])
                                  (<= (:x %)
                                      (:y %))))
                    ;;(take 1)
                    ))


(def tekstin-koko 40)

;; (def font (font/create-by-name "Serif" tekstin-koko))

(def dark-theme {:text-color [150 150 150 255]
                 :background-color [0 0 0 255]
                 :button-color [0 0 0 255]
                 :event-description-color [70 50 30]})

(def light-theme {:text-color [0 0 0 255]
                  :background-color [255 255 255 255]
                  :button-color [200 200 255 255]
                  :event-description-color [10 60 10]})
(def theme dark-theme)

(defn teksti [teksti & [koko väri]]
  (visuals/text-area (str teksti)
                     (if väri
                       väri
                       (:text-color theme))
                     (font/create-by-name "Serif" (or koko tekstin-koko))
                     #_(visuals/liberation-sans-regular (or koko tekstin-koko))))

(defn now []
  (System/currentTimeMillis))

(defn initialize-exercise [state exercise]
  (-> state
      (assoc :previous-exercise (:exercise state)
             :previous-options (:options state)
             :exercise-start-time (now)
             :exercise exercise
             :options (let [right-answer (* (:x exercise)
                                            (:y exercise))]
                        (->> (repeatedly (fn [] (max 2
                                                     (- (+ (inc (rand-int 20))
                                                           right-answer)
                                                        10))))
                             (remove #{right-answer})
                             (distinct)
                             (take 3)
                             (concat [right-answer])
                             (shuffle))))))



(defn next-exercise [state]
  (let [candidates (let [unfinished-exercises (remove (fn [exercise]
                                                        (<= maximum-exercise-points
                                                            (or (get-in state [:points exercise])
                                                                0)))
                                                      exercises)]
                     (if (= unfinished-exercises
                            [(:exercise state)])
                       [(:exercise state)]
                       (->> unfinished-exercises
                            (remove #{(:exercise state)})
                            (take 3))))]
    (when (not (empty? candidates))
      (rand-nth candidates))))

(def answer-keys [:a :s :d :f])

(def anwser-key-to-option-index (into {} (map vector answer-keys
                                              (range 4))))

(defonce points-atom (atom nil))

(comment
  (spit "lumon-time-table-points.edn" (pr-str @points-atom))
  )

(defn speed-points [duration]
  (int (max 0
            (Math/floor (/ (- 6000
                              duration)
                           1000)))))

(defn scores [state]
  (layouts/vertically-2 {:margin 10}
                        (teksti (str "Total answer points: " (apply + (vals (:points state)))))
                        (teksti (str "Total speed points: " (apply + (map (comp speed-points :duration)
                                                                          (filter :right-answer?
                                                                                  (:exercise-durations state))))))

                        (teksti (str "Right answers: " (count (filter :right-answer?
                                                                      (:exercise-durations state)))))
                        (teksti (str "Wrong answers: " (count (remove :right-answer?
                                                                      (:exercise-durations state)))))
                        (teksti (str "Passed execises: " (count (filter #(= % maximum-exercise-points)
                                                                        (vals (:points state))))))))

(defn- game-view  [state]
  (let [finish-phase (or (animation/phase! :finish 2000)
                         0)
        answer-animation-duration 2000
        wrong-answer-is-animating? (animation/animating? @animation/state-atom
                                                         :wrong-answer
                                                         answer-animation-duration)]
    (layouts/superimpose (visuals/rectangle-2 :fill-color
                                              (:background-color theme))
                         (layouts/center-horizontally

                          (when (> 1 finish-phase)
                            (layouts/with-margins (-> finish-phase
                                                      (animation/exponential-ease-in 3)
                                                      (animation/linear-mapping 0 2000))
                              0 0 0
                              (layouts/with-margin 50
                                (layouts/vertically-2 {:margin 20 :centered? true}
                                                      (let [{:keys [x y]} (if wrong-answer-is-animating?
                                                                            (:previous-exercise state)
                                                                            (:exercise state))]
                                                        (teksti (str x " * " y)))
                                                      (layouts/grid [(map (fn [value]
                                                                            (layouts/with-margin 10
                                                                              (teksti value tekstin-koko
                                                                                      (if wrong-answer-is-animating?
                                                                                        (let [right-answer (* (:x (:previous-exercise state))
                                                                                                              (:y (:previous-exercise state)))]
                                                                                          (cond (= right-answer value)
                                                                                                [0 150 0 255]

                                                                                                (= (:previous-answer state)
                                                                                                   value)
                                                                                                [150 0 0 255]

                                                                                                :else
                                                                                                (:text-color theme)))
                                                                                        (:text-color theme)))))
                                                                          (if wrong-answer-is-animating?
                                                                            (:previous-options state)
                                                                            (:options state)))
                                                                     (map (fn [anser-key]
                                                                            (layouts/with-margin 10 (teksti (name anser-key))))
                                                                          answer-keys)])
                                                      (let [exercise-ponts-view (fn [exercise]
                                                                                  (layouts/with-maximum-size 190 nil
                                                                                    (let [points (or (get (:points state)
                                                                                                          exercise)
                                                                                                     0)
                                                                                          row-height 45
                                                                                          corner-radius 20
                                                                                          block (fn [fill-color]
                                                                                                  (layouts/box 5
                                                                                                               (visuals/rectangle-2 :fill-color nil
                                                                                                                                    :draw-color [80 80 80 255]
                                                                                                                                    :line-width 2
                                                                                                                                    :corner-arc-radius corner-radius)
                                                                                                               (assoc (visuals/rectangle-2 :fill-color fill-color
                                                                                                                                           :corner-arc-radius corner-radius)
                                                                                                                      :width 50
                                                                                                                      :height (- row-height
                                                                                                                                 10))))]
                                                                                      (layouts/superimpose (layouts/horizontally-2 {:margin 5}
                                                                                                                                   (concat (repeat (abs points)
                                                                                                                                                   (block (let [opacity (if (and (or (animation/animating? @animation/state-atom
                                                                                                                                                                                                           :right-answer
                                                                                                                                                                                                           answer-animation-duration)
                                                                                                                                                                                     (animation/animating? @animation/state-atom
                                                                                                                                                                                                           :wrong-answer
                                                                                                                                                                                                           answer-animation-duration))
                                                                                                                                                                                 (= (:previous-exercise state)
                                                                                                                                                                                    exercise))
                                                                                                                                                                          (animation/sine 0 255 0.4
                                                                                                                                                                                          (:time @animation/state-atom))
                                                                                                                                                                          255)]
                                                                                                                                                            (if (< 0 points)
                                                                                                                                                              [0 80 0 opacity]
                                                                                                                                                              [80 0 0 opacity]))))
                                                                                                                                           (repeat (- maximum-exercise-points
                                                                                                                                                      (abs points))
                                                                                                                                                   (block [0 0 0 0]))))
                                                                                                           (layouts/with-maximum-size nil row-height
                                                                                                             (layouts/center (teksti (str (:x exercise) " * " (:y exercise)))))))))]
                                                        (layouts/horizontally-2 {:margin 50}
                                                                                (layouts/vertically-2 {:margin 5 :centered? true}
                                                                                                      (for [exercise (take (/ (count exercises)
                                                                                                                              2)
                                                                                                                           exercises)]
                                                                                                        (exercise-ponts-view exercise)))
                                                                                (layouts/vertically-2 {:margin 5 :centered? true}
                                                                                                      (for [exercise (drop (/ (count exercises)
                                                                                                                              2)
                                                                                                                           exercises)]
                                                                                                        (exercise-ponts-view exercise)))))
                                                      (let [corner-radius 20
                                                            block (fn [fill-color]
                                                                    (layouts/box 5
                                                                                 (visuals/rectangle-2 :fill-color nil
                                                                                                      :draw-color [80 80 80 255]
                                                                                                      :line-width 2
                                                                                                      :corner-arc-radius corner-radius)
                                                                                 (assoc (visuals/rectangle-2 :fill-color fill-color
                                                                                                             :corner-arc-radius corner-radius)
                                                                                        :width 50
                                                                                        :height 50)))]
                                                        (layouts/horizontally-2 {:margin 10
                                                                                 :end true}

                                                                                (for [exercise-duration (take-last 10 (:exercise-durations state))]
                                                                                  (let [speed-points (speed-points (:duration exercise-duration))]
                                                                                    (layouts/vertically-2 {:margin 2}
                                                                                                          (concat (repeat (- 5 speed-points)
                                                                                                                          (block [0 0 0 0]))
                                                                                                                  (repeat speed-points
                                                                                                                          (block (if (:right-answer? exercise-duration)
                                                                                                                                   [0 80 0 255]
                                                                                                                                   [80 0 0 255]))))))
                                                                                  #_(teksti exercise-duration))
                                                                                (let [remaining-speed-points (speed-points (- (now)
                                                                                                                              (:exercise-start-time state)))]
                                                                                  (when (< 0 remaining-speed-points)
                                                                                    (animation/swap-state! animation/set-wake-up 100))
                                                                                  (layouts/vertically-2 {:margin 2}
                                                                                                        (concat (repeat (- 5 remaining-speed-points)
                                                                                                                        (block [0 0 0 0]))
                                                                                                                (repeat remaining-speed-points
                                                                                                                        (block [0 80 0 255])))))
                                                                                ))
                                                      (scores state))))))

                         (when (animation/running? @animation/state-atom :finish)
                           (layouts/center-horizontally
                            (assoc (layouts/vertically-2 {:margin 10
                                                          :centered? true}
                                                         (teksti "Ready!")
                                                         (scores state))
                                   :y (animation/linear-mapping (animation/exponential-ease-out finish-phase
                                                                                                3)
                                                                -1500
                                                                300))))

                         (when (animation/animating? @animation/state-atom
                                                     :right-answer
                                                     answer-animation-duration)
                           (layouts/center-horizontally
                            (let [phase (animation/phase! :right-answer answer-animation-duration)
                                  color [0 150 0 (- 255
                                                    (-> (animation/exponential-ease-in phase 2)
                                                        (animation/linear-mapping 0 155)))]

                                  y (- 400 (-> (animation/exponential-ease-out phase 3)
                                               (animation/linear-mapping 0 300)))]
                              (when (> 1 phase)
                                (layouts/superimpose (assoc (teksti "Right!" 50 color)
                                                            :x 0
                                                            :y y)
                                                     (assoc (teksti "Right!" 50 color)
                                                            :x 700
                                                            :y y))))))

                         (when (animation/animating? @animation/state-atom
                                                     :wrong-answer
                                                     answer-animation-duration)
                           (layouts/center-horizontally
                            (let [phase (animation/phase! :wrong-answer answer-animation-duration)
                                  color [150 0 0 (- 255
                                                    (-> (animation/exponential-ease-in phase 2)
                                                        (animation/linear-mapping 0 155)))]

                                  y (+ 100 (-> (animation/exponential-ease-out phase 3)
                                               (animation/linear-mapping 0 300)))]
                              (when (> 1 phase)
                                (layouts/superimpose (assoc (teksti "Wrong!" 50 color)
                                                            :x 0
                                                            :y y)
                                                     (assoc (teksti "Wrong!" 50 color)
                                                            :x 700
                                                            :y y)))))))))

(defn initialize-state []
  (let [state {:score 0
               :exercise-durations []
               } #_{:points (read-string (slurp "lumon-time-table-points.edn"))}
        ]
    (initialize-exercise state
                         (next-exercise state))))

(defn event-handler [state-atom _node event]
  (let [state @state-atom]
    (when (and (= :key-pressed (:type event))
               (some #{(:key event)}
                     answer-keys))

      (if (:finished? state)
        (do (animation/swap-state! animation/delete :finish)
            (reset! state-atom (initialize-state)))
        (let [answer (get (:options state)
                          (anwser-key-to-option-index (:key event)))
              right-answer? (= (* (:x (:exercise state))
                                  (:y (:exercise state)))
                               answer)
              state (update-in state
                               [:points (:exercise state)]
                               (fn [points]
                                 (let [points (or points 0)
                                       points (if right-answer?
                                                (inc points)
                                                (dec points))]
                                   (-> points
                                       (max (- maximum-exercise-points))
                                       (min maximum-exercise-points)))))
              state (update state :exercise-durations conj
                            {:duration (- (now)
                                          (:exercise-start-time state))
                             :right-answer? right-answer?})
              next-exercise (next-exercise state)
              finished? (not (some? next-exercise))]

          (reset! state-atom
                  (-> state
                      (assoc :finished? finished?
                             :previous-answer answer)
                      (cond-> (some? next-exercise)
                        (initialize-exercise next-exercise))))

          (if finished?
            (animation/swap-state! animation/start :finish)
            (if right-answer?
              (animation/swap-state! animation/start :right-answer)
              (animation/swap-state! animation/start :wrong-answer)))

          (reset! points-atom
                  (:points @state-atom)))))))

(defn root-view []
  (let [state-atom (atom (initialize-state))]
    (fn []
      (let [state @state-atom]
        (keyboard/set-focused-event-handler! (partial event-handler state-atom))

        (game-view state)))))

(defonce event-channel-atom (atom nil))

(defn start []
  #_(aloita)

  (reset! event-channel-atom
          (application/start-application #'root-view
                                         :on-exit #(reset! event-channel-atom nil))))


(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))
