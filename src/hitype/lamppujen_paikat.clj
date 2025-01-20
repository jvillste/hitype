(ns hitype.lamppujen-paikat
  (:require [clojure.java.io :as io]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [flow-gl.gui.animation :as animation]
            [fungl.layouts :as layouts]
            [clojure.test :refer [deftest]]
            [clojure.pprint :as pprint]
            [fungl.component.text-area :as text-area]))

(def lamppuseinäsuhde (/ 1 2))

#_(defn paikat-rivissä [pituus määrä]
    (let [ jako (/ pituus (inc määrä))]
      {:jako (int jako)
       :paikat (for [index (range määrä)]
                 (int (* jako (inc index))))}))

;;  lamppujen-jako * (määrä - 1) + 2* suhde * lamppujen-jako = pituus
;;  lamppujen-jako * ( (määrä - 1) + 2* suhde) = pituus
;;  lamppujen-jako = pituus / ( (määrä - 1) + 2 * suhde)

(defn paikat-rivissä [pituus
                      määrä
                      lamppuseinäsuhde
                      lisäväli]
  (if (= määrä 1)
    {:kaikkien-lamppujen-etäisyydet-seinästä (list (int (/ pituus 2)))
     :kaikkien-lamppujen-etäisyydet-seinästä-lisävälin-kanssa (list (int (+ lisäväli
                                                                            (/ pituus 2))))}
    (let [lamppujen-välejä (dec määrä)
          lamppujen-etäisyys-toisistaan (/ pituus (+ lamppujen-välejä
                                                     (* 2 lamppuseinäsuhde)))
          ensimmäisen-lampun-etäisyys-seinästä (* lamppujen-etäisyys-toisistaan
                                                  lamppuseinäsuhde)
          kaikkien-lamppujen-etäisyydet-seinästä (map int
                                                      (concat [ensimmäisen-lampun-etäisyys-seinästä]
                                                              (for [index (range (dec määrä))]
                                                                (int (+ ensimmäisen-lampun-etäisyys-seinästä
                                                                        (* lamppujen-etäisyys-toisistaan (inc index)))))))]
      {:lamppujen-etäisyys-toisistaan (int lamppujen-etäisyys-toisistaan)
       :kaikkien-lamppujen-etäisyydet-seinästä kaikkien-lamppujen-etäisyydet-seinästä
       :kaikkien-lamppujen-etäisyydet-seinästä-lisävälin-kanssa (map int (map (partial + lisäväli)
                                                                              kaikkien-lamppujen-etäisyydet-seinästä))})))
(comment
  (paikat-rivissä 100 10 1))

(defn kaikki-paikat [huone]
  (let [lamppuseinäsuhde (or (:lamppuseinäsuhde huone)
                             lamppuseinäsuhde)
        lisäväli-mitattavaan-vaakaseinään (or (:lisäväli-mitattavaan-vaakaseinään huone)
                                              0)
        lisäväli-mitattavaan-pystyseinään (or (:lisäväli-mitattavaan-pystyseinään huone)
                                              0)]
    (assoc huone
           :etäisyydet-seinästä-vaakaan (paikat-rivissä (:leveys huone)
                                                        (:lamppuja-vaakaan huone)
                                                        lamppuseinäsuhde
                                                        lisäväli-mitattavaan-vaakaseinään)
           :etäisyydet-seinästä-pystyyn (paikat-rivissä (:pituus huone)
                                                        (:lamppuja-pystyyn huone)
                                                        lamppuseinäsuhde
                                                        lisäväli-mitattavaan-pystyseinään)
           :keskipiste {:x (long (+ (/ (:leveys huone)
                                       2)
                                    lisäväli-mitattavaan-vaakaseinään))
                        :y (long (+ (/ (:pituus huone)
                                       2)
                                    lisäväli-mitattavaan-pystyseinään))}
           :keskipiste-ilman-lisäväliä {:x (long (/ (:leveys huone)
                                                    2))
                                        :y (long (/ (:pituus huone)
                                                    2))})))

(def hirsipaneelin-paksuus 28)
(def paneelin-paksuus 15)
(def runkotolpan-paksuus 96)
(def seinän-paksuus (+ runkotolpan-paksuus
                       (* 2 paneelin-paksuus)))
(def kaapin-syvyys 600)
(def portaiden-leveys 910)

(def huoneet
  [{:nimi "pieni eteinen"
    :leveys (- 2076 seinän-paksuus kaapin-syvyys)
    :pituus 1640
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 1}

   {:nimi "pieni olohuone"
    :leveys 4500
    :pituus 4630
    :lamppuja-vaakaan 3
    :lamppuja-pystyyn 3
    :lamppuseinäsuhde 0.5}

   {:nimi "pieni keittiö"
    :leveys (- 2810
               hirsipaneelin-paksuus
               kaapin-syvyys)
    :pituus (- 3190
               kaapin-syvyys)
    :lamppuja-vaakaan 2
    :lamppuja-pystyyn 2
    :lamppuseinäsuhde 0.5
    :lisäväli-mitattavaan-pystyseinään kaapin-syvyys}

   {:nimi "pieni makuuhuone"
    :leveys (- 3680
               paneelin-paksuus
               kaapin-syvyys)
    :pituus 3190
    :lamppuja-vaakaan 2
    :lamppuja-pystyyn 2
    :lamppuseinäsuhde 0.3
    :lisäväli-mitattavaan-vaakaseinään (+ paneelin-paksuus
                                          kaapin-syvyys)}

   {:nimi "pieni kylpyhuone"
    :leveys (- 2000
               kaapin-syvyys)
    :pituus 2820
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 3
    :lamppuseinäsuhde 1}

   {:nimi "iso eteinen"
    :leveys (- 2410
               kaapin-syvyys)
    :pituus 3320
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 2
    :lamppuseinäsuhde 0.6
    :lisäväli-mitattavaan-vaakaseinään kaapin-syvyys}

   {:nimi "iso olohuone"
    :leveys 5450
    :pituus 3820
    :lamppuja-vaakaan 3
    :lamppuja-pystyyn 3
    :lamppuseinäsuhde 0.7
    :lisäväli-mitattavaan-vaakaseinään 2410}

   {:nimi "iso keittiö"
    :leveys (- 5478 portaiden-leveys)
    :pituus (- 3815 kaapin-syvyys)
    :lamppuja-vaakaan 3
    :lamppuja-pystyyn 2
    :lamppuseinäsuhde 0.5}

   {:nimi "iso työhuone"
    :leveys (- 4100 kaapin-syvyys)
    :pituus (- 4400 600)
    :lamppuja-vaakaan 2
    :lamppuja-pystyyn 3
    :lamppuseinäsuhde 0.5}

   {:nimi "iso portaikko"
    :leveys 910
    :pituus 3607
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 2
    :lamppuseinäsuhde 0.7}

   {:nimi "iso lastenhuone 1"
    :leveys (- 2994 kaapin-syvyys)
    :pituus 4165
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 3
    :lamppuseinäsuhde 0.7
    :lisäväli-mitattavaan-vaakaseinään kaapin-syvyys}

   {:nimi "iso lastenhuone 2"
    :leveys 4159
    :pituus 2865
    :lamppuja-vaakaan 3
    :lamppuja-pystyyn 1
    :lamppuseinäsuhde 0.7}

   {:nimi "iso aula"
    :leveys 4320
    :pituus 1250
    :lamppuja-vaakaan 2
    :lamppuja-pystyyn 1
    :lamppuseinäsuhde 0.5}

   {:nimi "iso vaatehuone"
    :leveys (- 1320 400)
    :pituus (- 2890 kaapin-syvyys)
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 1
    :lamppuseinäsuhde 0.5
    :lisäväli-mitattavaan-pystyseinään kaapin-syvyys}

   {:nimi "iso iso makkari"
    :leveys (- 5410 kaapin-syvyys)
    :pituus 2840
    :lamppuja-vaakaan 2
    :lamppuja-pystyyn 2
    :lamppuseinäsuhde 0.5
    :lisäväli-mitattavaan-vaakaseinään kaapin-syvyys}

   {:nimi "iso pieni vaatehuone"
    :leveys (- 1770 kaapin-syvyys)
    :pituus (- 1010 400)
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 1
    :lamppuseinäsuhde 0.5}

   {:nimi "iso kylpyhuone"
    :leveys 2050
    :pituus 1940
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 1
    :lamppuseinäsuhde 0.5}

   {:nimi "iso kodinhoitohuone"
    :leveys 1870
    :pituus 1390
    :lamppuja-vaakaan 1
    :lamppuja-pystyyn 1
    :lamppuseinäsuhde 0.5}])

(def huone (first (filter (fn [huone]
                            (= (:nimi huone)
                               #_"eteinen"
                               #_"makuuhuone"
                               #_"olohuone"
                               #_"portaikko"
                               #_"keittiö"
                               #_"työhuone"
                               #_"työhuoneen valaisinpistorasiat"
                               #_"kylpyhuone"
                               #_"iso lastenhuone 2"
                               #_"iso lastenhuone 2"
                               #_"iso aula"
                               #_"iso vaatehuone"
                               #_"iso iso makkari"
                               #_"iso pieni vaatehuone"
                               "iso kylpyhuone"
                               #_"iso kodinhoitohuone"))
                          (map kaikki-paikat
                               huoneet))))

(defn describe-huone [huone]
  (apply str (interpose "\n" [(:nimi huone)
                              (str "leveys: " (:leveys huone))
                              (str "pituus: " (:pituus huone))
                              (str "etäisyydet vaakaan lisävälin kanssa: " (vec (get-in huone [:etäisyydet-seinästä-vaakaan
                                                                                               :kaikkien-lamppujen-etäisyydet-seinästä-lisävälin-kanssa])))

                              (str "etäisyydet pystyyn lisävälin kanssa: " (vec (get-in huone [:etäisyydet-seinästä-pystyyn
                                                                                               :kaikkien-lamppujen-etäisyydet-seinästä-lisävälin-kanssa])))

                              (str "etäisyydet vaakaan ilman lisäväliä: " (vec (get-in huone [:etäisyydet-seinästä-vaakaan
                                                                                              :kaikkien-lamppujen-etäisyydet-seinästä])))

                              (str "etäisyydet pystyyn ilman lisäväliä: " (vec (get-in huone [:etäisyydet-seinästä-pystyyn
                                                                                              :kaikkien-lamppujen-etäisyydet-seinästä])))

                              (str "keskipiste: " (:keskipiste huone))
                              (str "keskipiste ilman lisäväliä: " (:keskipiste-ilman-lisäväliä huone))])))

(def scale 1/10)

(defn with-scale [value]
  (int (* scale value)))

(defn rectangle [x y width height]
  (layouts/transpose (with-scale x)
                     (with-scale y)
                     (-> (visuals/rectangle-2 :draw-color [255 255 255 255]
                                              :fill-color nil
                                              :line-width 2)
                         (assoc :width (with-scale width)
                                :height (with-scale height)))))

(defn marking [x y]
  (let [marking-width 60]
    (rectangle (int (- x
                       (/ marking-width 2)))
               (int (- y
                       (/ marking-width 2)))
               marking-width
               marking-width)))

(defn view [state-atom]
  (let [state-atom (atom {})]
    (fn []
      (animation/swap-state! animation/set-wake-up 2000)
      (let [marking-width 60]
        (layouts/vertically-2 {:margin 10}
                              (visuals/text (str (:nimi huone)))
                              (apply layouts/superimpose
                                     (rectangle (or (:lisäväli-mitattavaan-vaakaseinään huone)
                                                    0)
                                                (or (:lisäväli-mitattavaan-pystyseinään huone)
                                                    0)
                                                (:leveys huone)
                                                (:pituus huone))
                                     (marking (:x (:keskipiste huone))
                                              (:y (:keskipiste huone)))
                                     (for [lamppu-x (range (:lamppuja-vaakaan huone))
                                           lamppu-y (range (:lamppuja-pystyyn huone))]
                                       (marking (nth (get-in huone [:etäisyydet-seinästä-vaakaan :kaikkien-lamppujen-etäisyydet-seinästä-lisävälin-kanssa])
                                                     lamppu-x)
                                                (nth (get-in huone [:etäisyydet-seinästä-pystyyn :kaikkien-lamppujen-etäisyydet-seinästä-lisävälin-kanssa])
                                                     lamppu-y))))
                              (visuals/text (with-out-str (pprint/pprint huone))
                                            #_[255 255 255 255]))))))

(defn start []
  (application/start-application #'view))

#_(do
  #_(println (with-out-str (pprint/pprint huone)))
  (println (describe-huone huone)))
