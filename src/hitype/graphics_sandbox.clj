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
            [flow-gl.gui.keyboard :as keyboard]
            [fungl.cache :as cache]
            [java-time :as java-time]
            [time-literals.data-readers :as data-readers]
            [time-literals.read-write :as read-write]
            [clojure.test :refer :all]
            [clojure.set :as set])
  (:import java.time.LocalDate
           java.time.ZonedDateTime))

(read-write/print-time-literals-clj!)

(comment
  (java-time/zoned-date-time)
  (java-time/as #time/zoned-date-time "2020-11-29T05:51:40.878+02:00[Europe/Helsinki]"
                :hour)
  (java-time/as (java-time/local-time #time/zoned-date-time "2020-11-29T05:51:40.878+02:00[Europe/Helsinki]")
                :hour-of-day)

  (java-time/as #time/zoned-date-time "2020-11-29T05:51:40.878+02:00[Europe/Helsinki]"
                :hour-of-day)
  (java-time/zoned-date-time)


  ) ;; TODO: remove-me

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
           :margin 30}
          (mapv (partial assert-and-show-errors assert-piirrettävä)
                (flatten piirrettävät)))))

(defn rivitä [& kuvat]
  (apply layouts/flow kuvat))

(defn three-number-color [colors]
  (vec (concat (map #(long (* % (/ 255 100)))
                    colors)
               [255])))

(defn teksti [teksti & [koko väri]]
  #_(visuals/text (str teksti) {:color (if väri
                                         (three-number-color väri)
                                         [0 0 0 255])
                                :font-size (or koko 50)})
  (visuals/text-area (str teksti)
                     (if väri
                       (three-number-color väri)
                       [0 0 0 255])
                     (visuals/liberation-sans-regular (or koko 50))))

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

(defmacro piirrä-2 [& {:keys [tila alkuarvo näppäimistökäsittelijä kuva aloitus]}]
  `(do (reset! view-atom
               (let [~tila (dependable-atom/atom ~alkuarvo)]
                 (fn []
                   (when ~aloitus (~aloitus))

                   (fn []
                     (keyboard/set-focused-event-handler! (wrap-keyboard-event-hanlder ~näppäimistökäsittelijä))
                     ~kuva))))
       nil))

(def yhdistä str)


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

(comment
    (prn 'base-view (:aloitus @view-atom)) ;; TODO: remove-me

  ) ;; TODO: remove-me

(defn base-view []
  (fn []
    (animation/swap-state! animation/set-wake-up 1000)
    ;;(prn 'base-view @animation/state-atom) ;; TODO: remove-me
    @animation/state-atom
    (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                         (layouts/center (layouts/vertically-2 {:margin 10} [@view-atom])))))

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (application/start-window #'base-view))


(def explosion (let [original-frames (buffered-image/gif-frames (io/resource "explosion.gif"))]
                 (vec (concat (drop 7 original-frames)
                              (take 7 original-frames)))))

(defn image-animation [animation-key frames end-image]
  (let [phase (animation/phase! animation-key
                                1000)]
    (if (= 1 phase)
      end-image
      (visuals/image (get frames
                          (int (animation/linear-mapping (animation/ping-pong 0.3
                                                                              phase)
                                                         0
                                                         (dec (count frames)))))))))

(defn kirjoitusharjoitus []
  (piirrä-2 :tila
            tila

            :aloitus
            (funktio [] (animation/start! :merkin-räjäytys))

            :alkuarvo
            {:merkki ""
             :seuraava-merkki "A"
             :aloitusaika 0
             :ajat []
             :sydämmet 10}

            :näppäimistökäsittelijä
            (funktio [tapahtuma]
                     (if (= (.toLowerCase (:seuraava-merkki (hae tila)))
                            (.toLowerCase (:merkki tapahtuma)))
                       (do
                         (aseta! tila
                                 :seuraava-merkki
                                 (rand-nth (remove (funktio [merkki]
                                                            (= merkki
                                                               (:seuraava-merkki (hae tila))))
                                                   ["A" "H" "L" "S"])))

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
                         (animation/start! :merkin-räjäytys)

                         (aseta! tila :aloitusaika (aika)))
                       (if (< 1 (:sydämmet (hae tila)))
                         (vaihda! tila :sydämmet dec)
                         (aseta! tila :game-over true))))

            :kuva
            (if (:game-over (hae tila))
              (teksti "GAME OVER" 100 [100 0 0 100])
              (allekkain
               (image-animation :merkin-räjäytys
                                explosion
                                (teksti (:seuraava-merkki (hae tila))
                                        200))
               (teksti (int (- (aika)
                               (:aloitusaika (hae tila)))))
               (vierekkäin (for [aika (:ajat (hae tila))]
                             (teksti (int aika) 100
                                     (cond (< aika 5)
                                           [0 100 0 100]

                                           (< aika 8)
                                           [0 0 100 100]

                                           :default
                                           [100 0 0 100]))))
               (vierekkäin (repeat (:sydämmet (hae tila))
                                   (kuva "hero-tiles/heart.png" 100 100))
                           (repeat (- 10 (:sydämmet (hae tila)))
                                   (kuva "hero-tiles/gray-heart.png" 100 100))))))
  )

(def kysymykset [
                 {:kysymys "2 * 2", :vastaus "4", :luokka :kertolasku}
                 {:kysymys "2 * 3", :vastaus "6", :luokka :kertolasku}
                 {:kysymys "2 * 4", :vastaus "8", :luokka :kertolasku}
                 {:kysymys "2 * 5", :vastaus "10", :luokka :kertolasku}
                 {:kysymys "2 * 6", :vastaus "12", :luokka :kertolasku}
                 {:kysymys "2 * 7", :vastaus "14", :luokka :kertolasku}
                 {:kysymys "2 * 8", :vastaus "16", :luokka :kertolasku}
                 {:kysymys "2 * 9", :vastaus "18", :luokka :kertolasku}
                 {:kysymys "3 * 3", :vastaus "9", :luokka :kertolasku}
                 {:kysymys "3 * 4", :vastaus "12", :luokka :kertolasku}
                 {:kysymys "3 * 5", :vastaus "15", :luokka :kertolasku}
                 {:kysymys "3 * 6", :vastaus "18", :luokka :kertolasku}
                 {:kysymys "3 * 7", :vastaus "21", :luokka :kertolasku}
                 {:kysymys "3 * 8", :vastaus "24", :luokka :kertolasku}
                 {:kysymys "3 * 9", :vastaus "27", :luokka :kertolasku}
                 {:kysymys "4 * 4", :vastaus "16", :luokka :kertolasku}
                 {:kysymys "4 * 5", :vastaus "20", :luokka :kertolasku}
                 {:kysymys "4 * 6", :vastaus "24", :luokka :kertolasku}
                 {:kysymys "4 * 7", :vastaus "28", :luokka :kertolasku}
                 {:kysymys "4 * 8", :vastaus "32", :luokka :kertolasku}
                 {:kysymys "4 * 9", :vastaus "36", :luokka :kertolasku}
                 {:kysymys "5 * 5", :vastaus "25", :luokka :kertolasku}
                 {:kysymys "5 * 6", :vastaus "30", :luokka :kertolasku}
                 {:kysymys "5 * 7", :vastaus "35", :luokka :kertolasku}
                 {:kysymys "5 * 8", :vastaus "40", :luokka :kertolasku}
                 {:kysymys "5 * 9", :vastaus "45", :luokka :kertolasku}
                 {:kysymys "6 * 6", :vastaus "36", :luokka :kertolasku}
                 {:kysymys "6 * 7", :vastaus "42", :luokka :kertolasku}
                 {:kysymys "6 * 8", :vastaus "48", :luokka :kertolasku}
                 {:kysymys "6 * 9", :vastaus "54", :luokka :kertolasku}
                 {:kysymys "7 * 7", :vastaus "49", :luokka :kertolasku}
                 {:kysymys "7 * 8", :vastaus "56", :luokka :kertolasku}
                 {:kysymys "7 * 9", :vastaus "63", :luokka :kertolasku}
                 {:kysymys "8 * 8", :vastaus "64", :luokka :kertolasku}
                 {:kysymys "8 * 9", :vastaus "72", :luokka :kertolasku}
                 {:kysymys "9 * 9", :vastaus "81", :luokka :kertolasku}

                 {:kysymys "Mikä nimi lampaalle pitää antaa että se vaihtaa väriä koko ajan?", :vastaus "jeb_" :luokka :minecraft}
                 {:kysymys "Kuinka monta sydäntä on manaajalla?", :vastaus "24" :luokka :minecraft}
                 {:kysymys "Minne lampaita voi spawnata?", :vastaus "Ruoholle jonka valoisuus on yhdeksän tai enemmän." :luokka :minecraft}
                 ;; {:kysymys "Onko aurinko noussut?", :vastaus "ei"}
                 ;; {:kysymys "Onko äiti herännyt?", :vastaus "kyllä"}
                 ;; {:kysymys "1000 * 1000", :vastaus "1 000 000"}
                 ;; {:kysymys "yksi plus kaksi", :vastaus "kolme"}
                 {:kysymys "2 + 2", :vastaus "4" :luokka :lumo}
                 {:kysymys "1 - 1", :vastaus "0" :luokka :lumo}
                 {:kysymys "3 + 4", :vastaus "7" :luokka :lumo}
                 ])

(comment
  (vec (for [x (range 2 10)
             y (range 2 10)
             :when (<= x y)]
         {:kysymys (str x " * " y)
          :vastaus (str (* x y))
          :luokka :kertolasku}))
  (rand-nth #{1 2 3})
  )

(defn loki-tuloksiksi [loki]
  (->> loki
       (partition-by :kysymys)
       (mapcat (fn [log-lines]
                 (->> log-lines
                      (partition 3 1)
                      (map (fn [log-lines]
                             (let [[aloitus lopetus tulos] log-lines]
                               (when (and (= :näytettiin-kysymys (:tapahtuma aloitus))
                                          (= :näytettiin-vastaus (:tapahtuma lopetus))
                                          (#{:osattiin :ei-osattu} (:tapahtuma tulos)))
                                 {:aika (:aika aloitus)
                                  :kysymys (:kysymys aloitus)
                                  :miettimisaika (.toMillis (java.time.Duration/between (:aika aloitus)
                                                                                        (:aika lopetus)))
                                  :tulos (:tapahtuma tulos)}))))
                      (remove nil?))))))

(defn lisää-ajat-edelliseen-kertaan [tulokset]
  (->> tulokset
       (group-by :kysymys)
       (vals)
       (mapcat (fn [tulokset]
                 (let [tulokset (sort-by :aika tulokset)]
                   (->> tulokset
                        (partition 2 1)
                        (map (fn [[ensimmäinen toinen]]
                               (assoc toinen
                                      :millisekunteja-edelliseen-kertaan (.toMillis (java.time.Duration/between (:aika ensimmäinen)
                                                                                                                (:aika toinen))))))
                        (concat [(first tulokset)])))))))

(defn miettimisaika-osaamiseksi [miettimisaika]
  (float (if (< miettimisaika 5000)
           1
           (- 1
              (min 1
                   (/ (- miettimisaika 5000)
                      10000))))))

(deftest test-miettimisaika-osaamiseksi
  (is (= 1.0
         (miettimisaika-osaamiseksi 4000)))

  (is (= 0.5
         (miettimisaika-osaamiseksi 10000)))

  (is (= 0.0
         (miettimisaika-osaamiseksi 15000)))

  (is (= 0.0
         (miettimisaika-osaamiseksi 20000))))

(defn millisekunteja-edelliseen-kertaan-kertoimeksi [millisekunteja-edelliseen-kertaan]
  (if (<= millisekunteja-edelliseen-kertaan
          (* 2 60 1000))
    (* 0.5
       (/ millisekunteja-edelliseen-kertaan
          (* 2 60 1000)))
    (min 1.0
         (+ 0.5
            (* 0.5
               (/ millisekunteja-edelliseen-kertaan
                  (* 12 60 60 1000)))))))

(deftest test-millisekunteja-edelliseen-kertaan-kertoimeksi
  (is (= 0.0
         (millisekunteja-edelliseen-kertaan-kertoimeksi 0)))

  (is (= 0.5
         (millisekunteja-edelliseen-kertaan-kertoimeksi (* 2 60 1000))))

  (is (= 0.5014004629629629
         (millisekunteja-edelliseen-kertaan-kertoimeksi (+ 1000 (* 2 60 1000)))))

  (is (= 1.0
         (millisekunteja-edelliseen-kertaan-kertoimeksi (* 12 60 60 1000))))

  (is (= 1.0
         (millisekunteja-edelliseen-kertaan-kertoimeksi (+  1 (* 12 60 60 1000))))))

(defn osaaminen [tulos]
  (cond (= :ei-osattu (:tulos tulos))
        0

        (nil? (:millisekunteja-edelliseen-kertaan tulos))
        (miettimisaika-osaamiseksi (:miettimisaika tulos))

        :default
        (* (miettimisaika-osaamiseksi (:miettimisaika tulos))
           (millisekunteja-edelliseen-kertaan-kertoimeksi (:millisekunteja-edelliseen-kertaan tulos)))))

(deftest test-osaaminen
  (is (=
       (osaaminen {:kysymys {:kysymys "3 * 4", :vastaus "12"},
                   :miettimisaika 42896,
                   :tulos :osattiin}))))

(defn lisää-osaamiset-tuloksiin [tulokset]
  (map (fn [tulos]
         (assoc tulos :osaaminen (osaaminen tulos)))
       tulokset))

(defonce log (atom []))

(defn viimeisimmät-tulokset [tulokset]
  (->> tulokset
       (group-by :kysymys)
       (vals)
       (map (fn [tulokset]
              (last (sort-by :aika tulokset))))))


(defn viimeisimmät-tulokset-lokilta [loki]
  (->> loki
       loki-tuloksiksi
       lisää-ajat-edelliseen-kertaan
       lisää-osaamiset-tuloksiin
       viimeisimmät-tulokset))

(defn osaamiset [loki kysymykset]
  (let [tulokset-lokilta (viimeisimmät-tulokset-lokilta loki)]
    (concat tulokset-lokilta
            (->> (set/difference (set kysymykset)
                                 (set (map :kysymys tulokset-lokilta)))
                 (map (fn [kysymys]
                        {:kysymys kysymys
                         :osaaminen 0}))))))

(comment
  (def log (atom []))

  (viimeisimmät-tulokset-lokilta @log)

  (spit "temp/valon-kysymysloki" @log)
  (reset! log (eval (read-string (slurp "temp/valon-kysymysloki"))))

  (viimeisimmät-tulokset (lisää-osaamiset-tuloksiin (lisää-ajat-edelliseen-kertaan (loki-tuloksiksi @log))))
  )


(defn lokita! [tila tapahtuma]
  (swap! log
         conj
         {:aika (ZonedDateTime/now)
          :kysymys (:seuraava-kysymys (hae tila))
          :tapahtuma tapahtuma}))

(defn flash-cards []
  (piirrä-2 :tila
            tila

            :aloitus
            (funktio []
                     (lokita! tila :näytettiin-kysymys))

            :alkuarvo
            (let [kysymykset (set/union (set (->> (osaamiset @log
                                                             kysymykset)
                                                  (shuffle)
                                                  (remove (fn [tulos]
                                                            (= :lumo (-> tulos :kysymys :luokka))))
                                                  (filter (fn [tulos]
                                                            (< (:osaaminen tulos) 0.7)))

                                                  (map :kysymys)
                                                  (take 8))
                                             #_kysymykset
                                             #_(take 3 kysymykset))
                                        #_(set (->> (osaamiset @log
                                                               kysymykset)
                                                    (filter (fn [tulos]
                                                              (= :lumo (-> tulos :kysymys :luokka))))
                                                    (map :kysymys))))]
              {:jäljellä-olevat-kysymykset kysymykset
               :seuraava-kysymys (rand-nth (vec kysymykset))
               :tila :kysymys})

            :näppäimistökäsittelijä
            (funktio [tapahtuma]
                     (if (and (= :kysymys (:tila (hae tila)))
                              (= "k" (:merkki tapahtuma)))
                       (do (lokita! tila :näytettiin-vastaus)
                           (aseta! tila :tila :vastaus))
                       (case (:merkki tapahtuma)
                         "j" (do (lokita! tila :osattiin)
                                 (vaihda! tila
                                          (fn [tila]
                                            (let [jäljellä-olevat-kysymykset (disj (:jäljellä-olevat-kysymykset tila)
                                                                                   (:seuraava-kysymys tila))]
                                              (if (empty? jäljellä-olevat-kysymykset)
                                                (assoc tila
                                                       :tila :valmis
                                                       :jäljellä-olevat-kysymykset jäljellä-olevat-kysymykset)
                                                (assoc tila
                                                       :tila :kysymys
                                                       :jäljellä-olevat-kysymykset jäljellä-olevat-kysymykset
                                                       :seuraava-kysymys (rand-nth (vec jäljellä-olevat-kysymykset)))))))
                                 (lokita! tila :näytettiin-kysymys))
                         "k" (do (lokita! tila :ei-osattu)
                                 (vaihda! tila
                                          (fn [tila]
                                            (assoc tila
                                                   :tila :kysymys
                                                   :seuraava-kysymys (rand-nth (vec (:jäljellä-olevat-kysymykset tila))))))
                                 (lokita! tila :näytettiin-kysymys))
                         nil)))

            :kuva
            (let [tila (hae tila)]
              (allekkain
               (case (:tila tila)
                 :kysymys (allekkain (teksti (:kysymys (:seuraava-kysymys tila)))
                                     (teksti "Paina \"k\" nähdäksesi vastauksen."
                                             30))
                 :vastaus (allekkain (teksti (str (:kysymys (:seuraava-kysymys tila))
                                                  " = "
                                                  (:vastaus (:seuraava-kysymys tila))))
                                     (teksti (format "Aikaa kului: %.1f sekuntia" (double (/ (let [[aloitus lopetus] (take-last 2 @log)]
                                                                                               (.toMillis (java.time.Duration/between (:aika aloitus)
                                                                                                                                      (:aika lopetus))))
                                                                                             1000)))
                                             30)
                                     (teksti "Paina \"j\" jos osasit ja \"k\" jos et osannut."
                                             30)
                                     (teksti (str (count (:jäljellä-olevat-kysymykset tila)) " kysymystä jäljellä")
                                             30))
                 :valmis (allekkain (teksti "Opit kaikki!")
                                    (teksti "Osaamisesi:")
                                    (layouts/vertically-2 {}
                                                          (for [osaaminen (->> (osaamiset @log
                                                                                          (:jäljellä-olevat-kysymykset tila))
                                                                               (sort-by :osaaminen))]
                                                            (teksti (str (format "%.0f" (* 100 (double (:osaaminen osaaminen)))) "  =  " (:kysymys (:kysymys osaaminen)))
                                                                    30)))))))))


(comment
  (kirjoitusharjoitus)
  (flash-cards)

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
            :kuva (allekkain (teksti (yhdistä "Painoit " (hae merkki)))))

  ) ;; TODO: remove-me













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
