(ns hitype.graphics-sandbox
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [fungl.component.text-area :as text-area]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.util :as util]
            [hitype.util :as hitype-util]
            [flow-gl.gui.keyboard :as keyboard]))

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

(defmacro piirrä
  ([kuva]
   `(do (reset! view-atom
                (fn [] ~kuva))
        nil))

  ([tila alkuarvo & kuva]
   `(do (reset! view-atom
                (let [~tila (dependable-atom/atom ~alkuarvo)]
                  (fn [] ~@kuva)))
        nil))

  )

(defn wrap-keyboard-event-hanlder [handler]
  (fn [event]
    (when (= :key-released (:type event))
      (handler {:näppäin (:key event)
                :merkki (str (:character event))}))))

(defmacro piirrä-2 [& {:keys [tila alkuarvo näppäimistökäsittelijä kuva]}]
  `(do (reset! view-atom
               (let [~tila (dependable-atom/atom ~alkuarvo)]
                 (fn []
                   (keyboard/set-focused-event-handler! (wrap-keyboard-event-hanlder ~näppäimistökäsittelijä))
                   ~kuva)))
       nil))

(def yhdistä str)


(defn kirjoitusharjoitus []
  (piirrä-2 :tila
            tila

            :alkuarvo
            {:merkki ""
             :seuraava-merkki "a"
             :aloitusaika 0
             :ajat []}

            :näppäimistökäsittelijä
            (funktio [tapahtuma]
                     (when (= (:seuraava-merkki (hae tila))
                              (:merkki tapahtuma))
                       (aseta! tila
                               :seuraava-merkki
                               (rand-nth (remove (funktio [merkki]
                                                          (= merkki
                                                             (:seuraava-merkki (hae tila))))
                                                 ["a" "l" "h"])))
                       (aseta! tila
                               :merkki
                               (:merkki tapahtuma))

                       (vaihda! tila
                                :ajat
                                (funktio [kestot]
                                         (vec (take-last 5
                                                         (conj kestot
                                                               (round2 2 (- (aika)
                                                                            (:aloitusaika (hae tila)))))))))
                       (aseta! tila :aloitusaika (aika))))

            :kuva
            (allekkain
             (teksti (yhdistä "Paina " (:seuraava-merkki (hae tila))))
             (teksti (yhdistä "Aika " (round2 2 (- (aika)
                                                   (:aloitusaika (hae tila))))))
             (teksti (yhdistä "Ajat: " (:ajat (hae tila))))))
  )
(comment
  (kirjoitusharjoitus)
  (rand-nth ["a" "b" "c"])
  (piirrä-2 :tila merkki
            :alkuarvo ""
            :näppäimistökäsittelijä (funktio [tapahtuma]
                                             (aseta! merkki
                                                     (:merkki tapahtuma)))
            :kuva (teksti (yhdistä "Painoit " (hae merkki))))

  (piirrä-2 :tila merkki
            :alkuarvo ""
            :näppäimistökäsittelijä (funktio [tapahtuma]
                                             (aseta! merkki
                                                     (yhdistä (float (aika))
                                                              " "
                                                              (:merkki tapahtuma))))
            :kuva (teksti (yhdistä "Painoit " (hae merkki))))

  ) ;; TODO: remove-me



(defn aseta!
  ([tila arvo]
   (reset! tila arvo))

  ([tila avain-tai-polku arvo]
   (if (vector? avain-tai-polku)
     (swap! tila assoc-in avain-tai-polku arvo)
     (swap! tila assoc avain-tai-polku arvo))))

(defn vaihda!
  ([tila funktio]
   (swap! tila funktio))

  ([tila avain-tai-polku funktio]
   (if (vector? avain-tai-polku)
     (swap! tila update-in avain-tai-polku funktio)
     (swap! tila update avain-tai-polku funktio))))

(defn hae
  ([tila]
   @tila)
  ([tila avain-tai-polku]
   (if (vector? avain-tai-polku)
     (get-in @tila avain-tai-polku)
     (get @tila avain-tai-polku))))

(hitype-util/def-with-macro klikatessa [solmu funktio]
  (assoc solmu :mouse-event-handler (fn [node event]
                                      (when (= :mouse-clicked
                                               (:type event))
                                        (funktio))
                                      event)))

(defmacro tai [& vaihtoehdot]
  `(or ~@vaihtoehdot))

(defmacro funktio [& argumentit-ja-keho]
  `(fn ~@argumentit-ja-keho))

(defmacro määritele [nimi arvo]
  `(def ~nimi ~arvo))


(defn round2
  "Round a double to the given precision (number of significant digits)
  From https://stackoverflow.com/questions/10751638/clojure-rounding-to-decimal-places"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn aika []
  (animation/swap-state! animation/start-if-not-running :aika)
  (round2 2 (float (animation/phase! :aika))))


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


(defn pumppaa [tiedoston-nimi vaihe nopeus]
  (for [luku (luvut 1 10)]
    (kuva tiedoston-nimi
          (edes-takas 50 300 10 (+ luku
                                   vaihe
                                   (* nopeus (aika))))
          (edes-takas 5 200 10 (+ luku
                                  vaihe
                                  (* nopeus (aika)))))))
(comment

  (piirrä (herätä 1)
          (vierekkäin (repeatedly 10
                                  (fn [] (suorakaide 50 100 [(satunnaisluku 0 100)
                                                             (satunnaisluku 0 100)
                                                             (satunnaisluku 0 100)])))))

  (määritele värit ())

  (piirrä (vierekkäin (for [luku (luvut 0 10)]
                        (suorakaide 100 100
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

  (piirrä (rivitä (map (fn [luku]
                         (teksti (str " Lumo ")
                                 50 [luku 0 50]))
                       (luvut 1 100))))

  (piirrä (vierekkäin (kuva "kissa.jpg" (edes-takas 100 200 2 (aika)))
                      (kuva "kissa.jpg" (edes-takas 100 200 2 (+ 1 (aika))))))

  (piirrä (teksti (int (aika))))



  (piirrä (vierekkäin (allekkain (pumppaa "kissa.jpg" 0 2))
                      (allekkain (pumppaa "marsu2.jpg" 0 2)))
          #_(vierekkäin (allekkain (pumppaa "marsu.jpg" 0 5))
                        (rivitä (pumppaa "kissa.jpg" 5 5))))

  (piirrä (allekkain (for [luku (luvut 1 10)]
                       (suorakaide 1000
                                   (edes-takas 5 100 100
                                               (aika))
                                   [(* luku (/ 100 10))
                                    (- 100 (* luku (/ 100 10))) 0]))))

  (piirrä (vierekkäin (allekkain (pumppaa "marsu.jpg"))

                      #_(allekkain (vierekkäin (toista (edes-takas 1 5 5 (aika))
                                                       (teksti "JEE")))
                                   (vierekkäin (toista (edes-takas 1 5 5 (+ 1 (aika)))
                                                       (teksti "JUU"))))

                      (allekkain (pumppaa "kissa.jpg"))))
  )

(defn base-view []
  (fn []
    (animation/swap-state! animation/set-wake-up 1000)
    ;;(prn 'base-view @animation/state-atom) ;; TODO: remove-me
    @animation/state-atom
    (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                         (layouts/center (layouts/vertically-2 {:margin 10} (@view-atom))))))

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (application/start-window #'base-view))

















(comment

  ;; 1

  (piirrä tila
          1
          (teksti (hae tila)))

  ;; 2

  (piirrä tila 1
          (klikatessa (teksti (hae tila))
                      (vaihda! tila
                               (funktio [luku]
                                        (+ 1 luku)))))

  ;; 3

  (määritele lisää-yksi (funktio [luku]
                                 (+ 1 luku)))

  (piirrä tila 1
          (klikatessa (teksti (hae tila))
                      (vaihda! tila
                               lisää-yksi)))

  ;; 4

  (piirrä tila
          {:luku 1}
          (klikatessa (teksti (hae tila :luku))
                      (vaihda! tila
                               :luku
                               (funktio [luku]
                                        (+ 1 luku)))))



  (piirrä (kuva "marsu.jpg"))


  (piirrä (vierekkäin (kuva "marsu.jpg" 800)
                      (allekkain (kuva "kruunu.jpg" 800)
                                 (kuva "kissa.jpg" 800))))



  )
