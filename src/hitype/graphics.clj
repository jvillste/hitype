(ns hitype.graphics
  (:require [hitype.graphics-sandbox :refer :all]))

(comment

  (piirrä (allekkain (teksti "Valo")
                     (vierekkäin (allekkain (repeat 5 (teksti "Lumo")))
                                 (allekkain (repeat 5 (teksti "Valo"))))))

  (piirrä (vierekkäin (teksti "Valo")
                      (teksti "Valo")))

  (let [sana (read-line)]
    (piirrä (vierekkäin (teksti sana)
                        (teksti "  ")
                        (teksti "Valo3"))))

  )
