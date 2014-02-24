(ns lapwing.game.sections
  (:require [lapwing.util :as util :refer [indexed defs]]
            [lapwing.game.rooms :as rooms]
            [lapwing.entity :as entity]
            [lonocloud.synthread :as ->]))

(defs
  width         1
  height        1
  pixel-width   (* width rooms/pixel-width)
  pixel-height  (* height rooms/pixel-height))

(defn create-random
  []
  (rooms/realize-template
    (rooms/any)))

(defn create-first
  []
  (rooms/realize-template rooms/first-room))
