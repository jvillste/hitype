(ns hitype.music-xml
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
            [clojure.set :as set]
            [overtone.midi.file :as midi-file]
            [clojure.xml :as xml]
            [medley.core :as medley]))

(defn startparse-sax-non-validating [s ch]
  (.. (doto (. javax.xml.parsers.SAXParserFactory (newInstance))
        (.setValidating false)
        (.setFeature "http://apache.org/xml/features/nonvalidating/load-dtd-grammar" false)
        (.setFeature "http://apache.org/xml/features/nonvalidating/load-external-dtd" false)
        (.setFeature "http://xml.org/sax/features/validation" false)
        (.setFeature "http://xml.org/sax/features/external-general-entities" false)
        (.setFeature "http://xml.org/sax/features/external-parameter-entities" false))
      (newSAXParser) (parse s ch)))

(defn parts [score]
  (->> score
       :content
       (filter (fn [element]
                 (= :part (:tag element))))))

(defn measures [part]
  (->> part
       :content
       (filter (fn [element]
                 (= :measure (:tag element))))))

(defn measure-attributes [measure]
  (when-let [attributes (->> measure
                             :content
                             (filter (fn [element]
                                       (= :attributes (:tag element))))
                             (first))]
    ))


(comment
  (xml/parse "/Users/jukka/Downloads/final-countdown.xml")
  (->> (xml/parse "/Users/jukka/google-drive/jukka/music/testi.musicxml" startparse-sax-non-validating)
       parts
       (map measures))

  )
