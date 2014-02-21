(ns lapwing.game.entities
  (:require [lapwing.util :as util]
            [lonocloud.synthread :as ->]))

(let [wall-template
      {:pos {:x 0
             :y 0}
       :debug-rect "black"
       :hitbox {:width   48
                :height  48}
       :solid? true}]
  (defn wall
    [x y]
    (-> wall-template
      (->/in [:pos]
             (assoc :x x :y y)))))

(def player
  {:pos {:x 300
         :y 300}
   :vel {:x 0
         :y 0}
   :key-walker {:speed      300
                :can-walk?  true}
   :key-shooter {:can-shoot?  true
                 :shot-delay  0.2}
   :direction :right
   :debug-rect "red"
   :gravity true
   :hitbox {:width  48
            :height 48}
   :state-machine {:name   :player
                   :state  :falling}
   :player-jumper {:initial-acceleration     5500
                   :additional-acceleration  150
                   :additional-time          0.3}
   :dynamic-body {:stopped-by-solids? true}
   :camera-target true})

(let [shot-template
      {:pos {:x 0
             :y 0}
       :vel {:x 0
             :y 0}
       :debug-rect "green"
       :hitbox {:width 16
                :height 16}
       :dynamic-body {:stopped-by-solids? true}
       :collision-handler (fn [self other]
                            [[:destroy self]])}]
  (defn shot
    [x y direction]
    (-> shot-template
      (->/in [:pos]
             (assoc :x x :y y))
      (->/in [:vel]
             (assoc :x (* 500 (util/direction->int direction)))))))
