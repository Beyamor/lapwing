(ns lapwing.entities
  (:require [lapwing.entity :as entity]))

(defn create
  [initial-entities]
  (into {}
        (for [e initial-entities]
          [(entity/id e) e])))
