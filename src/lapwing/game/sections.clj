(ns lapwing.game.sections
  (:require [lapwing.util :as util :refer [indexed defs]]
            [lapwing.game.rooms :as rooms]
            [lapwing.entity :as entity]
            [lonocloud.synthread :as ->]))

(defs
  width         1
  height        2
  pixel-width   (* width rooms/pixel-width)
  pixel-height  (* height rooms/pixel-height))

(defn create
  [top bottom]
  (concat
    (rooms/realize-template top)
    (for [e (rooms/realize-template bottom)]
      (update-in e [:pos :y] + rooms/pixel-width))))
