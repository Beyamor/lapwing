(ns lapwing.core
  (:use seesaw.core))

(defn -main
  [& args]
  (invoke-later
    (-> (frame
          :title    "Lapwing"
          :content  "Wello horld"
          :on-close :exit)
      pack!
      show!)))
