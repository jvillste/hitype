(ns hitype.tekstiseikkailu
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [flow-gl.gui.animation :as animation]
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
            [clojure.set :as set]))


(defn ^:dynamic  onnistuiko? [todennäköisyys]
  (<= (rand)
      todennäköisyys))

(defn poista-komento [maailma paikka komentotunnus]
  (update-in maailma
             [paikka :komennot]
             (fn [komennot]
               (remove (fn [komento]
                         (= (:tunnus komento)
                            komentotunnus))
                       komennot))))

(defn lisää-kokemusta [maailma määrä]
  (-> maailma
      (assoc :kokemuspisteiden-lisäys määrä)
      (update :kokemuspisteet (partial + määrä))))

(def etsi-avain-ja-kolikko {:tunnus :etsi-avain-ja-kolikko
                            :kuvaus "Etsi kultainen avain ja kolikko"
                            :toteutus (fn [maailma]
                                        (let [poista-tämä-komento (fn [maailma]
                                                                    (poista-komento maailma :olohuone :etsi-avain-ja-kolikko))]
                                          (if (get-in maailma [:olohuone :avain-ja-kolikkko-on-löydetty])
                                            (-> maailma
                                                (assoc :tapahtuman-kuvaus "Ei löytynyt mitään.")
                                                (poista-tämä-komento))
                                            (-> maailma
                                                (assoc :tapahtuman-kuvaus "Löysit kultakolikon ja avaimen ja laitoit ne taskuun.")
                                                (update-in [:tavarat] concat [{:nimi "Kultakolikko"}
                                                                              {:nimi "Kulta-avain"}])
                                                (assoc-in [:olohuone :avain-ja-kolikkko-on-löydetty] true)
                                                (lisää-kokemusta 10)
                                                (poista-tämä-komento)))))})

(def katso-telkkaria {:tunnus :katso-telkkaria
                      :kuvaus "Katso telkkaria"
                      :toteutus (fn [maailma]
                                  (if (onnistuiko? 0.33)
                                    (-> maailma
                                        (assoc :tapahtuman-kuvaus "Telkkarissa ääni sanoo: \"Etsi olohuoneesta kultainen avain ja kultakolikko.\"")
                                        (poista-komento :olohuone :etsi-avain-ja-kolikko)
                                        (update-in [:olohuone :komennot] conj etsi-avain-ja-kolikko)
                                        (lisää-kokemusta 5))
                                    (-> maailma
                                        (assoc :tapahtuman-kuvaus "Telkkarista tulee ryhmä hauta.")
                                        (lisää-kokemusta 1))))})

(def heitä-whattaswip {:tunnus :heitä-whattaswip
                       :kuvaus "Heitä whattaswip"
                       :toteutus (fn [maailma]
                                   (if (onnistuiko? 0.2)
                                     (-> maailma
                                         (assoc :tapahtuman-kuvaus "Onnistuit heittämään whattaswipin!!")
                                         (lisää-kokemusta 20))
                                     (-> maailma
                                         (assoc :tapahtuman-kuvaus "Pullo ei pysynyt pystyssä.")
                                         (lisää-kokemusta 1))))})

(def istuta-porkkanoita {:tunnus :istuta-porkkanoita
                         :kuvaus "Istuta porkkanoita"
                         :toteutus (fn [maailma]
                                     (update-in maailma [:takapiha] assoc :porkkanat-on-istutettu-vuorolla (:pelivuoro maailma)))})

(def paikat {:olohuone {:nimi "Olohuone"
                        :menonimi "olohuoneeseen"
                        :kuvaus "Täällä on sohva ja telkkari. Lattialla on matto."
                        :komennot [katso-telkkaria]
                        :exits [:keittiö]}
             :keittiö {:nimi "Keittiö"
                       :menonimi "keittiöön"
                       :kuvaus "Täällä on pöytä jolla on whattaswippipullo."
                       :komennot [heitä-whattaswip]
                       :exits [:olohuone]}
             #_:takapiha #_{:nimi "Takapiha"
                          :menonimi "takapihalle"
                          :kuvaus-funktio (fn [maailma]
                                            (str "Täällä on kasvimaa."
                                                 (when-let [istutus-vuoro (get-in maailma [:takapiha :porkkanat-on-istutettu-vuorolla])]
                                                   "Porkkanat on istutettu " (- (:pelivuoro maailma)
                                                                                istutus-vuoro) " vuoroa sitten.")
                                                 ))
                          :komennot [heitä-whattaswip]
                          :exits [:olohuone]}})

(def komentonäppäimet ["j" "k" "l" "d" "s" "a" "ö"])

(defn komentokartta [komennot]
  (into {}
        (map vector
             komentonäppäimet
             komennot)))

(deftest test-komentokartta
  (is (= "j"
         (keys (komentokartta [katso-telkkaria])))))

(defn valitse-komento [komentokartta]
  (loop []
    (println "Anna komennon kirjain:")
    (let [kirjain (read-line)]
      (if-let [komento (get komentokartta kirjain)]
        komento
        (recur)))))

(defn alusta-maailma []
  (merge paikat
         {:kokemuspisteet 0
          :pelaajan-paikka :olohuone
          :pelivuoro 1}))

(defn pelaajan-paikka [maailma]
  (get maailma (:pelaajan-paikka maailma)))

(defn mahdolliset-komennot [maailma]
  (let [paikka (pelaajan-paikka maailma)]
    (if (:peli-on-lopetettu maailma)
      [{:tunnus :aloita
        :kuvaus "Aloita uusi peli"
        :toteutus (fn [maailma]
                    (alusta-maailma))}]
      (concat (:komennot paikka)
              (for [paikan-tunnus (:exits paikka)]
                (let [menonimi (get-in maailma [paikan-tunnus :menonimi])]
                  {:tunnus [:mene paikan-tunnus]
                   :kuvaus (str "Mene " menonimi)
                   :toteutus (fn [maailma]
                               (-> maailma
                                   (assoc :pelaajan-paikka paikan-tunnus)
                                   (assoc :tapahtuman-kuvaus (str "Menit " menonimi))))}))
              [{:tunnus :lopeta
                :kuvaus "Lopeta"
                :toteutus (fn [maailma]
                            (assoc maailma :peli-on-lopetettu true))}]))))

(defn maailma-komentojen-jälkeen [& komentotunnukset]
  (loop [maailma (alusta-maailma)
         komentotunnukset komentotunnukset]
    (if-let [komentotunnus (first komentotunnukset)]
      (recur ((:toteutus (first (filter (fn [komento]
                                          (= (:tunnus komento)
                                             komentotunnus))
                                        (mahdolliset-komennot maailma))))
              maailma)
             (rest komentotunnukset))
      maailma)))

(deftest testit
  (is (= "Ei löytynyt mitään."
         (binding [onnistuiko? (constantly true)]
           (:tapahtuman-kuvaus (maailma-komentojen-jälkeen :katso-telkkaria
                                                           :etsi-avain-ja-kolikko
                                                           :katso-telkkaria
                                                           :etsi-avain-ja-kolikko)))))

  (is (= "Telkkarista tulee ryhmä hauta."
         (binding [onnistuiko? (constantly false)]
           (:tapahtuman-kuvaus (maailma-komentojen-jälkeen :katso-telkkaria
                                                           :katso-telkkaria))))))

(defn tilannekuvaus [maailma]
  (let [paikka (pelaajan-paikka maailma)
        komentokartta (komentokartta (mahdolliset-komennot maailma))
        rivit ["-------------------------"
               (if-let [tapahtuman-kuvaus (:tapahtuman-kuvaus maailma)]
                 [tapahtuman-kuvaus
                  ""]
                 nil)
               (:nimi paikka)
               (:kuvaus paikka)
               (if (not (empty? (:tavarat maailma)))
                 [""
                  "Sinulla on:"
                  (for [tavara (:tavarat maailma)]
                    (str "  " (:nimi tavara)))]
                 nil)
               ""
               "Komennot:"
               (for [[näppäin komento] komentokartta]
                 (str (.toUpperCase näppäin) " : " (:kuvaus komento)))]]
    (remove nil? (flatten rivit))))


(comment
  (println (string/join "\n"
                        (tilannekuvaus (maailma-komentojen-jälkeen :katso-telkkaria))))
  )

(defn aloita []
  (loop [maailma (alusta-maailma)]
    (let [paikka (pelaajan-paikka maailma)
          komentokartta (komentokartta (mahdolliset-komennot maailma))]
      (println (string/join "\n"
                            (tilannekuvaus maailma)))
      (let [komento (valitse-komento komentokartta)
            maailma ((:toteutus komento) (dissoc maailma :tapahtuman-kuvaus))]
        (if (:peli-on-lopetettu maailma)
          (println "Peli loppui.")
          (recur maailma))))))



;; GUI


(defn three-number-color [colors]
  (vec (concat (map #(long (* % (/ 255 100)))
                    colors)
               [255])))

(defn teksti [teksti & [koko väri]]
  (visuals/text-area (str teksti)
                     (if väri
                       (three-number-color väri)
                       [0 0 0 255])
                     (visuals/liberation-sans-regular (or koko 50))))

(def tekstin-koko 25)

(defn nappi [text]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color [220 220 255 255]
                                    :corner-arc-radius 30)
               (layouts/with-minimum-size 300 nil
                 (teksti text tekstin-koko))))



(defn tapahtuman-kuvaus [kuvaus]
  (teksti kuvaus tekstin-koko [0 0 100]))

(defn komentonäkymä [maailma]
  (for [[näppäin komento] (komentokartta (mahdolliset-komennot maailma))]
    (nappi (str (.toUpperCase näppäin) " : " (:kuvaus komento)))))

(defn world-view [maailma]
  (let [paikka (pelaajan-paikka maailma)]
    (layouts/with-margins 20 20 20 20
      (apply layouts/vertically-2 {:margin 10 :centered true}
             (if (:peli-on-lopetettu maailma)
               [(layouts/vertically-2 {:margin 10}
                                      (tapahtuman-kuvaus (str "Peli loppui. Sait " (:kokemuspisteet maailma) " kokemuspistettä " (dec (:pelivuoro maailma)) ":ssa pelivuorossa!"))
                                      (tapahtuman-kuvaus (str "Se on " (int (/ (:kokemuspisteet maailma)
                                                                               (dec (:pelivuoro maailma)))) " pistettä per vuoro.")))
                (komentonäkymä maailma)]
               [(teksti (:nimi paikka) (* 2 tekstin-koko))
                (layouts/vertically-2 {:margin 10}
                                      (tapahtuman-kuvaus (:tapahtuman-kuvaus maailma))
                                      (when-let [kokemuspisteiden-lisäys (:kokemuspisteiden-lisäys maailma)]
                                        (tapahtuman-kuvaus (str "Sait " kokemuspisteiden-lisäys " pistettä lisää kokemusta!")))
                                      (teksti (:kuvaus paikka) tekstin-koko))
                (if (not (empty? (:tavarat maailma)))
                  (layouts/vertically-2 {}
                                        (teksti "Sinulla on:" (* 1.5 tekstin-koko))
                                        (for [tavara (:tavarat maailma)]
                                          (layouts/with-margins 0 0 0 20
                                            (teksti (:nimi tavara) tekstin-koko))))
                  nil)
                (komentonäkymä maailma)
                (teksti (str "Kokemuspisteet: "(:kokemuspisteet maailma)) tekstin-koko)
                (teksti (str "Pelivuoro: "(:pelivuoro maailma)) tekstin-koko)])))))

(defn näppäimistökäsittelijä [state-atom event]
  (when (= :key-released (:type event))
    (when-let [komento (get (komentokartta (mahdolliset-komennot @state-atom))
                            (str (:character event)))]
      (swap! state-atom (fn [maailma]
                          (-> maailma
                              (assoc :kokemuspisteiden-lisäys nil)
                              (update :pelivuoro inc))))
      (swap! state-atom (:toteutus komento))

      (if (>= (:kokemuspisteet @state-atom)
              100)
        (swap! state-atom #(assoc % :peli-on-lopetettu true))))))

(defn root-view []
  (let [state-atom (atom (alusta-maailma))]
    (fn []
      (animation/swap-state! animation/set-wake-up 1000)
      @animation/state-atom
      (keyboard/set-focused-event-handler! (partial näppäimistökäsittelijä
                                                    state-atom))
      (world-view @state-atom))))

(defn start []
  #_(aloita)

  (application/start-window #'root-view :window (application/create-window 400 800)))
