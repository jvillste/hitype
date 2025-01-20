(ns hitype.tekstiseikkailu
  (:require
   [clojure.test :refer :all]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.layouts :as layouts]
   [flow-gl.graphics.font :as font]
   [clojure.core.async :as async]))

(defn ^:dynamic  onnistuiko? [todennäköisyys]
  (<= (rand)
      todennäköisyys))


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

(def theme {:text-color [0 0 0 255]
            :background-color [255 255 255 255]
            :button-color [200 200 255 255]
            :event-description-color [10 60 10]})

(defn teksti [teksti & [koko väri]]
  (visuals/text-area (str teksti)
                     (if väri
                       (three-number-color väri)
                       (:text-color theme))
                     (font/create-by-name "Serif" (or koko tekstin-koko))
                     #_(visuals/liberation-sans-regular (or koko tekstin-koko))))




(defn nappi [text]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color (:button-color theme)
                                    :corner-arc-radius 30)
               (layouts/with-minimum-size 300 nil
                 (teksti text
                         tekstin-koko))))

(defn tapahtuman-kuvaus [kuvaus]
  (teksti kuvaus tekstin-koko (:event-description-color theme)))

(defn komentonäkymä [komennot]
  (for [[näppäin komento] (komentokartta komennot)]
    (nappi (str (.toUpperCase näppäin) " : " (:kuvaus komento)))))

(defn world-view [maailma]
  (let [paikka (pelaajan-paikka maailma)]
    (layouts/superimpose (visuals/rectangle-2 :fill-color
                                              (:background-color theme))
                         (layouts/with-maximum-size 700 nil
                           (layouts/with-margins 20 20 20 20
                             (apply layouts/vertically-2 {:margin 10 :centered? true}
                                    (if (:peli-on-lopetettu maailma)
                                      [(layouts/vertically-2 {:margin 10}
                                                             (tapahtuman-kuvaus (:lopetusviesti maailma))
                                                             (tapahtuman-kuvaus (str "Sait " (:kokemuspisteet maailma) " kokemuspistettä " (dec (:pelivuoro maailma)) " pelivuorossa!"))
                                                             (tapahtuman-kuvaus (str "Se on " (int (/ (:kokemuspisteet maailma)
                                                                                                      (dec (:pelivuoro maailma)))) " pistettä per vuoro.")))
                                       (komentonäkymä {:tunnus :roll
                                                       :kuvaus "Roll"
                                                       :toteutus (fn [_maailma]
                                                                   (alusta-maailma))})]
                                      [(teksti (:nimi paikka) (* 2 tekstin-koko))
                                       (layouts/vertically-2 {:margin 10}
                                                             (tapahtuman-kuvaus (:tapahtuman-kuvaus maailma))
                                                             (when-let [kokemuspisteiden-lisäys (:kokemuspisteiden-lisäys maailma)]
                                                               (tapahtuman-kuvaus (str "Sait " kokemuspisteiden-lisäys " pistettä lisää kokemusta!")))
                                                             (when-let [energian-muutos (:energian-muutos maailma)]
                                                               (if (< energian-muutos 0)
                                                                 (tapahtuman-kuvaus (str "Menetit " energian-muutos " pistettä energiaa!"))
                                                                 (tapahtuman-kuvaus (str "Sait " energian-muutos " pistettä lisää energiaa!"))))
                                                             (teksti (paikan-kuvaus maailma)))
                                       (if (not (empty? (:tavarat maailma)))
                                         (layouts/vertically-2 {}
                                                               (teksti "Sinulla on:" (* 1.5 tekstin-koko))
                                                               (for [tavara (:tavarat maailma)]
                                                                 (layouts/with-margins 0 0 0 20
                                                                   (teksti (:nimi tavara)))))
                                         nil)
                                       (komentonäkymä maailma)
                                       (teksti (str "Kokemuspisteet: " (:kokemuspisteet maailma)))
                                       (teksti (str "Energia: " (:energia maailma)))
                                       (teksti (str "Pelivuoro: "(:pelivuoro maailma)))])))))))




(defn näppäimistökäsittelijä [state-atom _node event]
  (prn event))

(defn edes-takas [mistä mihin kesto aika]
  (float (animation/linear-mapping (animation/ping-pong kesto aika)
                                   mistä
                                   mihin)))

(defn round2
  "Round a double to the given precision (number of significant digits)
  From https://stackoverflow.com/questions/10751638/clojure-rounding-to-decimal-places"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn aika []
  (animation/swap-state! animation/start-if-not-running :aika)
  (round2 2 (float (animation/phase! :aika))))

(defn rare-animation []
  (layouts/superimpose (let [liikkeen-määrä 15
                             keskipiste 400
                             tärinän-kesto 0.2]
                         (assoc (teksti  (let [texts ["YOU DON'T KNO%"
                                                      "YOU D_N'7 KNOW"
                                                      "YO? DON'L KNOW"
                                                      "YOU DON'P #NOW"]]
                                           (get texts
                                                (int (* (dec (count texts))
                                                        (mod (* 3.5 (aika))
                                                             1)))))

                                         100 [50 50 20 100])
                                :x (edes-takas (- keskipiste liikkeen-määrä)
                                               keskipiste
                                               tärinän-kesto
                                               (aika))

                                :y (edes-takas (- keskipiste liikkeen-määrä)
                                               keskipiste
                                               (* 0.9 tärinän-kesto)
                                               (aika))))))

(defn root-view []
  (let [state-atom (atom {})]
    (keyboard/set-focused-event-handler! (partial näppäimistökäsittelijä
                                                  state-atom))
    (fn []
      (layouts/superimpose (visuals/rectangle-2 :fill-color [0 0 0 255])
                           (teksti "moi" 200 [50 50 20 100])
                           #_(rare-animation)))))

(defonce event-channel-atom (atom nil))

(defn start []
  #_(aloita)

  (reset! event-channel-atom
          (application/start-application #'root-view
                                         :on-exit #(reset! event-channel-atom nil))))


(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))
