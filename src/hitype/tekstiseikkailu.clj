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
            [clojure.set :as set]
            [medley.core :as medley]))


(defn ^:dynamic  onnistuiko? [todennäköisyys]
  (<= (rand)
      todennäköisyys))

(defn poista-komento [maailma paikka komentotunnus]
  (update-in maailma
             [:paikat paikka :komennot]
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
                                          (if (get-in maailma [:paikat :olohuone :avain-ja-kolikkko-on-löydetty])
                                            (-> maailma
                                                (assoc :tapahtuman-kuvaus "Ei löytynyt mitään.")
                                                (poista-tämä-komento))
                                            (-> maailma
                                                (assoc :tapahtuman-kuvaus "Löysit kultakolikon ja avaimen ja laitoit ne taskuun.")
                                                (update-in [:tavarat] concat [{:nimi "Kultakolikko"}
                                                                              {:nimi "Kulta-avain"}])
                                                (assoc-in [:paikat :olohuone :avain-ja-kolikkko-on-löydetty] true)
                                                (lisää-kokemusta 10)
                                                (poista-tämä-komento)))))})

(def katso-telkkaria {:tunnus :katso-telkkaria
                      :kuvaus "Katso telkkaria"
                      :toteutus (fn [maailma]
                                  (if (onnistuiko? 0.33)
                                    (-> maailma
                                        (assoc :tapahtuman-kuvaus "Telkkarissa ääni sanoo: \"Etsi olohuoneesta kultainen avain ja kultakolikko.\"")
                                        (poista-komento :olohuone :etsi-avain-ja-kolikko)
                                        (update-in [:paikat :olohuone :komennot] conj etsi-avain-ja-kolikko)
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
                                     (-> maailma
                                         (assoc-in [:paikat :takapiha :porkkanoidenistutusvuoro]
                                                   (:pelivuoro maailma))
                                         (assoc :tapahtuman-kuvaus "Istutit porkkanat")
                                         (poista-komento :takapiha :istuta-porkkanoita)))})

(def maksimienergia 15)

(defn lisää-energiaa [maailma energiamäärä]
  (update maailma :energia (fn [energia]
                             (min maksimienergia
                                  (+ energia energiamäärä)))))

(def syö-porkkanat {:tunnus :syö-porkkanat
                    :kuvaus "Syö porkkanat"
                    :toteutus (fn [maailma]
                                (-> maailma
                                    (assoc-in [:paikat :takapiha :porkkanoidenistutusvuoro]
                                              nil)
                                    (assoc :tapahtuman-kuvaus "Söit porkkanat")
                                    (lisää-energiaa (int (/ maksimienergia 2)))
                                    (poista-komento :takapiha :syö-porkkanat)
                                    (update-in [:paikat :takapiha :komennot] conj istuta-porkkanoita)
                                    (lisää-kokemusta 20)))})

(def porkkanoiden-kypsymisaika 5)

(defn porkkanoiden-ikä [maailma]
  (when-let [istutus-vuoro (get-in maailma [:paikat :takapiha :porkkanoidenistutusvuoro])]
    (- (:pelivuoro maailma)
       istutus-vuoro)))

(defn mene [paikan-tunnus maailma]
  (-> maailma
      (assoc :pelaajan-paikka paikan-tunnus)
      (assoc :tapahtuman-kuvaus (str "Menit " (get-in maailma [:paikat paikan-tunnus :menonimi])))))

(declare paikat)

(defn menokomento [paikan-tunnus]
  {:tunnus [:mene paikan-tunnus]
   :kuvaus (str "Mene " (get-in paikat [paikan-tunnus :menonimi]))
   :toteutus (partial mene paikan-tunnus)})

(def paikat {:olohuone {:nimi "Olohuone"
                        :menonimi "olohuoneeseen"
                        :kuvaus "Täällä on sohva ja telkkari. Lattialla on matto."
                        :komennot [katso-telkkaria
                                   (menokomento :keittiö)]
                        ;;:exits [:keittiö]
                        }
             :keittiö {:nimi "Keittiö"
                       :menonimi "keittiöön"
                       :kuvaus "Täällä on pöytä jolla on whattaswippipullo."
                       :komennot [heitä-whattaswip
                                  (menokomento :olohuone)
                                  (menokomento :takapiha)]}
             :takapiha {:nimi "Takapiha"
                        :menonimi "takapihalle"
                        :kuvausfunktio (fn [maailma]
                                         (str "Täällä on kasvimaa."
                                              (when-let [ikä (porkkanoiden-ikä maailma)]
                                                (str " Porkkanat on istutettu " ikä " vuoroa sitten."
                                                     (if (>= ikä porkkanoiden-kypsymisaika)
                                                       " Ne voi nyt syödä!"
                                                       (str " Porkkanat voi syödä " (- porkkanoiden-kypsymisaika ikä) " vuoron päästä."))))))
                        :päivitysfunktio (fn [maailma]
                                           (if-let [ikä (porkkanoiden-ikä maailma)]
                                             (if (= ikä porkkanoiden-kypsymisaika)
                                               (update-in maailma [:paikat :takapiha :komennot] conj syö-porkkanat)
                                               maailma)
                                             maailma))
                        :komennot [istuta-porkkanoita
                                   (menokomento :keittiö)]}})

(def komentonäppäimet ["j" "f" "k" "d" "l" "s" "ö" "a"])

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
  (merge {:paikat paikat
          :kokemuspisteet 0
          :pelaajan-paikka :olohuone
          :tavarat []
          :pelivuoro 1
          :energia maksimienergia}))

(defn pelaajan-paikka [maailma]
  (get-in maailma [:paikat (:pelaajan-paikka maailma)]))


#_(defn exits [maailma]
  (:exits (pelaajan-paikka maailma)))

(defn mahdolliset-komennot [maailma]
  (let [paikka (pelaajan-paikka maailma)]
    (if (:peli-on-lopetettu maailma)
      [{:tunnus :aloita
        :kuvaus "Aloita uusi peli"
        :toteutus (fn [maailma]
                    (alusta-maailma))}]
      (concat (:komennot paikka)
              #_(for [paikan-tunnus (:exits paikka)]
                (let [menonimi (get-in maailma [paikan-tunnus :menonimi])]
                  {:tunnus [:mene paikan-tunnus]
                   :kuvaus (str "Mene " menonimi)
                   :toteutus (partial mene paikan-tunnus)}))
              [{:tunnus :odota
                :kuvaus "Odota"
                :toteutus (fn [maailma]
                            (assoc maailma :tapahtuman-kuvaus "Odotit yhden vuoron."))}
               {:tunnus :lopeta
                :kuvaus "Lopeta"
                :toteutus (fn [maailma]
                            (-> maailma
                                (assoc :peli-on-lopetettu true)
                                (assoc :lopetusviesti "Lopetit pelin.")))}]))))

(defn päivitä-paikat [maailma]
  (reduce (fn [maailma paikan-tunnus]
            (if-let [päivitysfunktio (get-in maailma [:paikat paikan-tunnus :päivitysfunktio])]
              (päivitysfunktio maailma)
              maailma))
          maailma
          (keys (:paikat maailma))))

(defn valmistaudu-komentoon [maailma]
  (assoc maailma :kokemuspisteiden-lisäys nil))

(defn lopeta-jos-kokemusta-on-riittävästi [maailma]
  (if (>= (:kokemuspisteet maailma)
          100)
    (-> maailma
        (assoc :peli-on-lopetettu true)
        (assoc :lopetusviesti "Peli loppui koska sait 100 kokemuspistettä"))
    maailma))

(defn lopeta-jos-energia-loppui [maailma]
  (if (< (:energia maailma)
         1)
    (-> maailma
        (assoc :peli-on-lopetettu true)
        (assoc :lopetusviesti "Kuolit nälkään."))
    maailma))

(defn edistä-aikaa [maailma]
  (-> maailma
      (update :pelivuoro inc)
      (update :energia dec)
      (päivitä-paikat)
      (lopeta-jos-kokemusta-on-riittävästi)
      (lopeta-jos-energia-loppui)))

(defn aja-komento [komento maailma]
  (-> maailma
      (valmistaudu-komentoon)
      ((:toteutus komento))
      edistä-aikaa))

(defn maailma-komentojen-jälkeen [& komentotunnukset]
  (loop [maailma (alusta-maailma)
         komentotunnukset komentotunnukset]
    (if-let [komentotunnus (first komentotunnukset)]
      (recur (if-let [komento (first (filter (fn [komento]
                                               (= (:tunnus komento)
                                                  komentotunnus))
                                             (mahdolliset-komennot maailma)))]
               (aja-komento komento maailma)
               (throw (Exception. (str "Et voi antaa komentoa " komentotunnus))))
             (rest komentotunnukset))
      maailma)))

(defn paikan-kuvaus [maailma]
  (let [paikka (pelaajan-paikka maailma)]
    (if-let [kuvausfunktio (:kuvausfunktio paikka)]
      (kuvausfunktio maailma)
      (:kuvaus paikka))))

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
                                                           :katso-telkkaria)))))

  (is (= "Söit porkkanat"
         (:tapahtuman-kuvaus (maailma-komentojen-jälkeen [:mene :keittiö]
                                                         [:mene :takapiha]
                                                         :istuta-porkkanoita
                                                         :odota
                                                         :odota
                                                         :odota
                                                         :odota
                                                         :syö-porkkanat)))))


;; GUI


(defn three-number-color [colors]
  (vec (concat (map #(long (* % (/ 255 100)))
                    colors)
               [255])))

(def tekstin-koko 25)

(defn teksti [teksti & [koko väri]]
  (visuals/text-area (str teksti)
                     (if väri
                       (three-number-color väri)
                       [0 0 0 255])
                     (visuals/liberation-sans-regular (or koko tekstin-koko))))



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
                                      (tapahtuman-kuvaus (:lopetusviesti maailma))
                                      (tapahtuman-kuvaus (str "Sait " (:kokemuspisteet maailma) " kokemuspistettä " (dec (:pelivuoro maailma)) " pelivuorossa!"))
                                      (tapahtuman-kuvaus (str "Se on " (int (/ (:kokemuspisteet maailma)
                                                                               (dec (:pelivuoro maailma)))) " pistettä per vuoro.")))
                (komentonäkymä maailma)]
               [(teksti (:nimi paikka) (* 2 tekstin-koko))
                (layouts/vertically-2 {:margin 10}
                                      (tapahtuman-kuvaus (:tapahtuman-kuvaus maailma))
                                      (when-let [kokemuspisteiden-lisäys (:kokemuspisteiden-lisäys maailma)]
                                        (tapahtuman-kuvaus (str "Sait " kokemuspisteiden-lisäys " pistettä lisää kokemusta!")))
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
                (teksti (str "Pelivuoro: "(:pelivuoro maailma)))])))))




(defn näppäimistökäsittelijä [state-atom event]
  (when (= :key-released (:type event))
    (when-let [komento (get (komentokartta (mahdolliset-komennot @state-atom))
                            (str (:character event)))]
      (swap! state-atom (partial aja-komento komento)))))

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
