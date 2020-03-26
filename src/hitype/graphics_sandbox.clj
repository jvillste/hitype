(ns hitype.graphics-sandbox
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]))

(defn load-image* [file-name]
  (if-let [resource (io/resource file-name)]
    (buffered-image/create-from-file (.getPath resource))
    (throw (ex-info (str "Tiedostoa " file-name " ei löydy")
                    {}))))

(def load-image (memoize load-image*))

(defn value-description [value]
  (cond (map? value)
        "piirrettävä"

        (string? value)
        (str "teksti \"" value "\"")

        (number? value)
        (str "numero \"" value "\"")

        (seq? value)
        "lista"))

(defn assert-type [assertion type-name value]
  (if (and (some? value)
           (not (assertion value)))
    (throw (ex-info (str (value-description value) " ei ole " type-name)
                    {}))
    value))

(defn assert-piirrettävä [value]
  (assert-type map? "piirrettävä" value))

(defn assert-väri [value]
  (assert-type (fn [value]
                 (and (vector? value)
                      (= 3 (count value))))
               "väri"
               value))

(defn assert-numero [value]
  (assert-type number?
               "numbero"
               value))

(defn show-errors* [body-function]
  (try
    (body-function)
    (catch Exception exception
      (.printStackTrace exception)
      (layouts/box 10
                   (visuals/rectangle-2 :draw-color [255 0 0 255]
                                        :line-width 2
                                        :fill-color [0 0 0 0]
                                        :corner-arc-radius 30)
                   (layouts/with-maximum-size 300 nil (visuals/text-area (str (.getMessage exception))
                                                                         [255 0 0 255]))))))

(defmacro show-errors [& body]
  `(show-errors* (fn [] ~@body)))

(defn assert-and-show-errors [assertion value]
  (show-errors (assertion value)))

(defn allekkain [& piirrettävät]
  (show-errors
   (apply layouts/vertically-2
          {:centered true
           :margin 10}
          (mapv (partial assert-and-show-errors assert-piirrettävä)
                (flatten piirrettävät)))))



(comment
  (mapv (fn [value]
          (show-errors (assert-piirrettävä value)))
        [1])

  ((comp show-errors* assert-piirrettävä) 1)
  (show-errors* (fn []
                  (mapv assert-piirrettävä
                        [1])))
  ) ;; TODO: remove-me


(defn vierekkäin [& piirrettävät]
  (show-errors
   (apply layouts/horizontally-2
          {:centered true
           :margin 10}
          (mapv (partial assert-and-show-errors assert-piirrettävä)
                (flatten piirrettävät)))))

(defn rivitä [& kuvat]
  (apply layouts/flow kuvat))

(defn three-number-color [colors]
  (vec (concat (map #(long (* % (/ 255 100)))
                    colors)
               [255])))

(def teksti (fn [teksti & [koko väri]]
              (visuals/text (str teksti) {:color (if väri
                                                   (three-number-color väri)
                                                   [0 0 0 255])
                                          :font-size (or koko 50)})))

(defn- assoc-if-not-nil [map key value]
  (if value
    (assoc map key value)
    map))

(defn kuva [tiedoston-nimi & [leveys korkeus]]
  (show-errors
   (let [image (visuals/image (load-image tiedoston-nimi))]
     (if (and (some? leveys)
              (not (some? korkeus)))
       (let [scale (/ leveys
                      (max (:width image)
                           (:height image)))]
         (-> image
             (update :width (comp int *) scale)
             (update :height (comp int *) scale)))

       (-> image
           (assoc-if-not-nil :width leveys)
           (assoc-if-not-nil :height korkeus))))))

(defn toista [lukumäärä toistettava]
  (repeat lukumäärä toistettava))

(defonce view-atom (atom (fn [] (visuals/rectangle-2))))

(defmacro piirrä [& kuva]
  `(do (reset! view-atom
               (fn [] ~@kuva))
       nil))

(defn aika []
  (animation/swap-state! animation/start-if-not-running :aika)
  (animation/phase! :aika))

(defn edes-takas [mistä mihin kesto aika]
  (float (animation/linear-mapping (animation/ping-pong kesto aika)
                                   mistä
                                   mihin)))

(defn luvut [mistä mihin]
  (range mistä (inc mihin)))



(defn suorakaide [leveys korkeus väri]
  (assert-numero leveys)
  (assert-numero korkeus)
  (assert-väri väri)
  (assoc (visuals/rectangle-2 :corner-arc-radius 20
                              :fill-color (three-number-color väri))
         :width leveys
         :height korkeus))

(defn satunnaisluku [mistä mihin]
  (long (+ mistä (rand mihin))))


(defn herätä [sekunnit]
  (animation/swap-state! animation/set-wake-up
                         (* sekunnit 1000)))

(comment

  (piirrä (herätä 1)
          (vierekkäin (repeatedly 10
                                  (fn [] (suorakaide 50 100 [(satunnaisluku 0 100)
                                                             (satunnaisluku 0 100)
                                                             (satunnaisluku 0 100)])))))

  (piirrä (vierekkäin (for [luku (luvut 0 10)]
                        (suorakaide 50 100
                                    [0
                                     (* luku 10)
                                     (edes-takas 0 100 5 (aika))]))))

  (defn kissanumero [numero]
    (allekkain (kuva "kissa.jpg" (* 30 numero))
               (teksti numero)))

  (piirrä (rivitä (map kissanumero (luvut 1 9))))

  (piirrä (rivitä (toista 100 (teksti "Moi" ))))







  (piirrä (allekkain (teksti "Moi")
                     "Moi"
                     (teksti "Moi")
                     3
                     (kuva "kissa.jpg" 200)
                     (kuva "kisa.jpg" 200)))

  (piirrä (rivitä (toista 100 "Moi")))

  (piirrä (vierekkäin (map kissanumero (luvut 1 9))))

  (piirrä (allekkain (toista 100 (teksti "ui9jki9knfk8i.bbbbkhkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" 10 [100 0 0]))))

  (piirrä (rivitä (map (fn [luku]
                         (teksti (str " " luku)
                                 50 [luku 0 50]))
                       (luvut 1 100))))

  (piirrä (vierekkäin (kuva "kissa.jpg" (edes-takas 100 200 2 (aika)))
                      (kuva "kissa.jpg" (edes-takas 100 200 2 (+ 1 (aika))))))

  (piirrä (teksti (int (aika))))

  (defn pumppaa [tiedoston-nimi vaihe nopeus]
    (for [luku (luvut 1 10)]
      (kuva tiedoston-nimi
            (edes-takas 50 300 10 (+ luku
                                     vaihe
                                     (* nopeus (aika))))
            (edes-takas 5 200 10 (+ luku
                                    vaihe
                                    (* nopeus (aika)))))))

  (piirrä (vierekkäin (allekkain (pumppaa "marsu.jpg" 0 2))
                      (allekkain (pumppaa "marsu2.jpg" 5 2)))
          #_(vierekkäin (allekkain (pumppaa "marsu.jpg" 0 5))
                        (rivitä (pumppaa "kissa.jpg" 5 5))))

  (piirrä (allekkain (for [luku (luvut 1 10)]
                       (suorakaide 500
                                   (edes-takas 5 100 10
                                               (aika))
                                   [(* luku (/ 100 10))
                                    (- 100 (* luku (/ 100 10))) 0]))))

  (piirrä (vierekkäin (allekkain (pumppaa "marsu.jpg"))

                      #_(allekkain (vierekkäin (toista (edes-takas 1 5 5 (aika))
                                                       (teksti "JEE")))
                                   (vierekkäin (toista (edes-takas 1 5 5 (+ 1 (aika)))
                                                       (teksti "JUU"))))

                      (allekkain (pumppaa "kissa.jpg")))))

(defn base-view []


  (animation/swap-state! animation/set-wake-up 1000)
  (prn 'base-view @animation/state-atom) ;; TODO: remove-me
  @animation/state-atom
  (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                       (layouts/center (layouts/vertically-2 {:margin 10} (@view-atom)))))

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (application/start-window base-view))
