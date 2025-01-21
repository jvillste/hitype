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

(defn three-number-color [colors]
  (vec (concat (map #(long (* % (/ 255 100)))
                    colors)
               [255])))

(def tekstin-koko 40)

;; (def font (font/create-by-name "Serif" tekstin-koko))

(def dark-theme {:text-color [150 150 150 255]
                 :background-color [30 30 30 255]
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



(def exercises (filter #(and (some  #{4} [(:x %) (:y %)])
                             (<= (:x %)
                                 (:y %)))
                       (for [x (range 2 10)
                             y (range 2 10)]
                         {:x x :y y})))

(defn initialize-exercise [state exercise]
  (-> state
      (assoc :exercise exercise
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

(def maximum-exercise-points 3)

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
  (spit "time-table-points.edn" (pr-str @points-atom))
  )

(defn- game-view  [state]
  (let [finish-phase (or (animation/repeating-phase! :finish 2000)
                         0)]
    (layouts/superimpose (visuals/rectangle-2 :fill-color
                                              (:background-color theme))
                         (layouts/center-horizontally

                          (when (> 1 finish-phase)
                            (layouts/with-margins (-> finish-phase
                                                      (animation/exponential-ease-in 8)
                                                      (animation/linear-mapping 0 2000))
                              0 0 0
                              (layouts/with-margin 50
                                (let [{:keys [x y]} (:exercise state)]
                                  (layouts/vertically-2 {:margin 20 :centered? true}
                                                        (teksti (str x " * " y))
                                                        (layouts/grid [(map (fn [value]
                                                                              (layouts/with-margin 10 (teksti value))) (:options state))
                                                                       (map (fn [anser-key]
                                                                              (layouts/with-margin 10 (teksti (name anser-key))))
                                                                            answer-keys)])
                                                        (layouts/horizontally-2 {:margin 50}
                                                                                (layouts/vertically-2 {:margin 5 :centered? true}
                                                                                                      (for [exercise (take (/ (count exercises)
                                                                                                                              2)
                                                                                                                           exercises)]
                                                                                                        (teksti (str (:x exercise) " * " (:y exercise)
                                                                                                                     " : "
                                                                                                                     (or (get (:points state)
                                                                                                                              exercise)
                                                                                                                         0)
                                                                                                                     #_" pistettä." ))))
                                                                                (layouts/vertically-2 {:margin 5 :centered? true}
                                                                                                      (for [exercise (drop (/ (count exercises)
                                                                                                                              2)
                                                                                                                           exercises)]
                                                                                                        (teksti (str (:x exercise) " * " (:y exercise)
                                                                                                                     " : "
                                                                                                                     (or (get (:points state)
                                                                                                                              exercise)
                                                                                                                         0)
                                                                                                                     #_" pistettä." )))))
                                                        (teksti (str "Yhteensä: " (apply + (vals (:points state)))))))))))

                         (when (animation/running? @animation/state-atom :finish)
                           (layouts/center-horizontally
                            (assoc (teksti "Valmis!")
                                   :y (animation/linear-mapping (animation/exponential-ease-out finish-phase
                                                                                                3)
                                                                -1500
                                                                300))))


                         (when (and (not (:finished? state))
                                    (animation/running? @animation/state-atom  :right-answer))
                           (layouts/center-horizontally
                            (let [phase (animation/phase! :right-answer 2000)
                                  color [150 150 150 (- 255
                                                        (-> (animation/exponential-ease-in phase 2)
                                                            (animation/linear-mapping 0 155)))]

                                  y (- 400 (-> (animation/exponential-ease-out phase 3)
                                               (animation/linear-mapping 0 300)))]
                              (when (> 1 phase)
                                (layouts/superimpose (assoc (teksti "Jee" 50 color)
                                                            :x 0
                                                            :y y)
                                                     (assoc (teksti "Jee" 50 color)
                                                            :x 500
                                                            :y y)))))))))


(def initial-state {:points (read-string (slurp "time-table-points.edn"))})

(defn event-handler [state-atom _node event]
  (let [state @state-atom]
    (when (and (= :key-pressed (:type event))
               (some #{(:key event)}
                     answer-keys))
      (let [{:keys [x y] :as exercise} (:exercise state)
            right-answer? (= (* x y)
                             (get (:options state)
                                  (anwser-key-to-option-index (:key event))))]
        (when right-answer?

          (animation/swap-state! animation/start :right-answer))

        (swap! state-atom (fn [state]
                            (let [state (update-in state
                                                   [:points exercise]
                                                   (if right-answer?
                                                     (fnil inc 0)
                                                     (fnil dec 0)))]
                              (if-some [exercise (next-exercise state)]
                                (initialize-exercise state
                                                     exercise)
                                (do (animation/swap-state! animation/start :finish)
;;                                     (.start (Thread. (fn []
;;                                                        (prn "sleeping")

;;                                                        (Thread/sleep 5000)
;;                                                        (prn "resetting")
;;                                                        (try
;;                                                          (animation/swap-state! animation/delete :finish)
;;                                                          (catch Exception e
;;                                                              (prn e) ;; TODO: remove me
;; ))                                                       (prn "phase " (animation/phase! :finish 1000))
;;                                                        (reset! state-atom initial-state))))
                                    (assoc state :finished? true))))))

        (reset! points-atom (:points @state-atom))))))

(defn root-view []
  (let [state-atom (atom (let [state initial-state]
                           (initialize-exercise state
                                                (next-exercise state))))]
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
