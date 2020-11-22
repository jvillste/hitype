(ns hitype.tekstiseikkailu
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
            [clojure.test :refer :all]
            [clojure.string :as string]))

(defn poista-komento [maailma paikka komentotunnus]
  (update-in maailma
             [paikka :komennot]
             (fn [komennot]
               (remove (fn [komento]
                         (= (:tunnus komento)
                            komentotunnus))
                       komennot))))

(defn etsi-avain-ja-kolikko-toteutus [maailma]
  (let [poista-tämä-komento (fn [maailma]
                              (poista-komento maailma :olohuone :etsi-avain-ja-kolikko))]
    (if (get-in maailma [:olohuone :avain-ja-kolikkko-on-löydetty])
      (-> maailma
          (assoc :tapahtuman-kuvaus "Ei löytynyt mitään.")
          (poista-tämä-komento))
      (-> maailma
          (assoc :tapahtuman-kuvaus "Löysit kultakolikon ja avaimen ja laitoit ne taksuun.")
          (update-in [:tavarat] concat [{:nimi "Kultakolikko"}
                                        {:nimi "Kulta-avain"}])
          (assoc-in [:olohuone :avain-ja-kolikkko-on-löydetty] true)
          (poista-tämä-komento)))))

(def etsi-avain-ja-kolikko {:tunnus :etsi-avain-ja-kolikko
                            :kuvaus "Etsi kultainen avain ja kolikko"
                            :toteutus etsi-avain-ja-kolikko-toteutus})

(defn ^:dynamic  onnistuiko? [todennäköisyys]
  (<= (rand)
      todennäköisyys))

(comment
  (repeatedly 10 #(onnistuiko? 0.3))
  ) ;; TODO: remove-me

(def katso-telkkaria {:tunnus :katso-telkkaria
                      :kuvaus "Katso telkkaria."
                      :toteutus (fn [maailma]
                                  (if (onnistuiko? 0.33)
                                    (-> maailma
                                        (assoc :tapahtuman-kuvaus "Telkkarissa ääni sanoo: \"Etsi olohuoneesta kultainen avain ja kultakolikko.\"")
                                        (poista-komento :olohuone :etsi-avain-ja-kolikko)
                                        (update-in [:olohuone :komennot] conj etsi-avain-ja-kolikko))
                                    (assoc maailma :tapahtuman-kuvaus "Telkkarista tulee ryhmä hauta.")))})

(def paikat {:olohuone {:nimi "Olohuone"
                        :kuvaus "Täällä on sohva ja telkkari. Lattialla on matto."
                        :komennot [katso-telkkaria]}})

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
         {:taskut []
          :pelaajan-paikka :olohuone}))

(defn pelaajan-paikka [maailma]
  (get maailma (:pelaajan-paikka maailma)))

(defn mahdolliset-komennot [maailma]
  (concat (:komennot (pelaajan-paikka maailma))
          [{:tunnus :lopeta
            :kuvaus "Lopeta"
            :toteutus (fn [maailma]
                        (assoc maailma :peli-on-lopetettu true))}]))

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

(defn start []
  (aloita))
