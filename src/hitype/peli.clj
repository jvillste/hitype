(ns hitype.peli
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(defn start-anna-nimesi []
  (println "Anna nimesi")
  (let [nimi (read-line)]
    (doseq [line-number (range 11)]
      (println (str line-number " Moi " nimi "!")))))

(defn weighted-random [probabilities]
  (let [target-number (rand)]
    (loop [sum 0
           probabilities probabilities]
      (when-let [propability (first probabilities)]
        (let [sum (+ sum (second propability))]
          (if (<= target-number
                  sum)
            (first propability)
            (recur sum
                   (rest probabilities))))))))

(def all-things (let [things [{:name "uncommon"
                               :probability 1/6}
                              {:name "rare"
                               :probability 1/12}
                              {:name "epic"
                               :probability 1/20}
                              {:name "mythical"
                               :probability 1/30}
                              {:name "legendary"
                               :probability 1/40}
                              {:name "super rare"
                               :probability 1/65}
                              {:name "ultra epic"
                               :probability 1/100}
                              {:name "ultra super rare"
                               :probability 1/120}
                              {:name "super legendary"
                               :probability 1/165}
                              {:name "ultra legendary"
                               :probability 1/200}
                              {:name "ultra super legendary"
                               :probability 1/295}
                              {:name "exotic"
                               :probability 1/999}
                              {:name "exotical"
                               :probability 1/1500}

                              {:name "waterical"
                               :probability 1/1999}
                              {:name "super exotic"
                               :probability 1/2222}
                              {:name "ultra exotic"
                               :probability 1/2555}
                              {:name "ultra super rare waterical"
                               :probability 1/3000}
                              {:name "hero"
                               :probability 1/3500}
                              {:name "super hero"
                               :probability 1/4000}
                              {:name "ultra hero"
                               :probability 1/4500}
                              {:name "ultra super rare hero"
                               :probability 1/5000}
                              {:name "ROBOTICAL"
                               :probability 1/7000}

                              {:name "CYBORDICAL" :probability 1/7500}
                              {:name "NATURAL" :probability 1/8000}
                              {:name "SUPERTUPRA" :probability 1/9345}
                              {:name "CRYSTALICIAL" :probability 1/9899}
                              {:name "EXOTIC APEX" :probability 1/9999}
                              {:name "TACEBLUE" :probability 1/10000}
                              {:name "SKIPPER" :probability 1/12000}
                              {:name "TRAPPER" :probability 1/12555}
                              {:name "TRUMBER" :probability 1/12999}
                              {:name "CLICCER" :probability 1/13000}
                              {:name "JADE" :probability 1/13555}
                              {:name "TRADE" :probability 1/14000}
                              {:name "FLAME" :probability 1/14555}
                              {:name "TRADETICAL" :probability 1/15000}
                              {:name "SKIPERITICAL" :probability 1/15555}
                              {:name "ULTRASKIP" :probability 1/16000}
                              {:name "FLAME ULTRA" :probability 1/16555}
                              {:name "TRARGET" :probability 1/17000}
                              {:name "FRASTECT" :probability 1/17555}
                              {:name "ULITRASKIP" :probability 1/18000}
                              {:name "SUPARICA" :probability 1/18555}
                              {:name "GLITZ" :probability 1/19000}
                              {:name "z0!%?L" :probability 1/25555}]]
                  (conj things
                        {:name "common"
                         :probability (- 1 (apply + (map :probability things)))})))

(defn random-thing []
  (if-let [thing (weighted-random (for [thing all-things]
                                    [thing (:probability thing)]))]
    thing
    (last all-things)))

(defn start []
  (loop [things #{}]
    (println "You have" (if (empty? things)
                          "nothing"
                          (string/join ", " (map :name things))))
    (println "You are missing" (if (empty? (set/difference (set all-things)
                                                           (set things)))
                                 "nothing"
                                 (string/join ", " (map :name (sort-by :name
                                                                       (set/difference (set all-things)
                                                                                       (set things)))))))
    (println "Press enter to roll. Enter \"quit\" to quit.")
    (let [command (read-line)]
      (if (= "quit" command)
        nil
        (let [thing (random-thing)]
          (println)
          (println (str "You got " (:name thing) " (" (:probability thing) ")!"))
          (recur (conj things thing)))))))

(comment
  (start)
  )
