(ns lapwing.game.entities
  (:require [lapwing.util :as util :refer [mmerge]]
            [lonocloud.synthread :as ->]))

(def unit-width 24)

(util/defs
  pos {:pos {:x 0
             :y 0}}

  vel {:vel {:x 0
             :y 0}}

  dynamic-body (mmerge pos vel
                       {:dynamic-body {:stopped-by-solids? true}}))

(defn square-hitbox
  [dim]
  {:hitbox {:width dim
            :height dim}})

(def unit-hitbox (square-hitbox unit-width))

(defn set-pos
  [e x y]
  (->/in e [:pos]
         (assoc :x x :y y)))

(let [wall-template
      (mmerge pos
              unit-hitbox
              {:type :wall
               :debug-rect "#1A4469"
               :solid? true
               :remove-when-passed? true
               :destructible? true})]
  (defn wall
    [x y]
    (-> wall-template
      (set-pos x y))))

(def player
  (mmerge dynamic-body
          unit-hitbox
          {:pos {:x 100
                 :y 100}
           :type :player
           :key-walker {:speed      300
                        :can-walk?  true}
           :key-shooter {:shot-delay  0.2}
           :bomb-thrower {:throw-delay 0.2}
           :bomb-holder 999999999999
           :direction :right
           :debug-rect "#ED95BA"
           :gravity true
           :state-machine {:name            :player
                           :initial-state   :falling}
           :player-jumper {:initial-acceleration     4000
                           :additional-acceleration  100
                           :additional-time          0.1}
           :camera-target true
           :gem-collector true}))

(let [shot-template
      (mmerge dynamic-body
              (square-hitbox 16)
              {:debug-rect "#F0D148"
               :collision-handler (fn [self other]
                                    [[:destroy self]])
               :remove-when-offscreen? true})]
  (defn shot
    [x y direction]
    (-> shot-template
      (set-pos x y)
      (->/in [:vel]
             (assoc :x (* 500 (util/direction->int direction)))))))

(def the-beast
  (mmerge dynamic-body
          {:type :beast
           :pos {:x -100}
           :vel {:x 0}
           ;:vel {:x 80}
           :hitbox {:width 100
                    :height 600}
           :debug-rect "#6CBDF0"
           :dynamic-body {:stopped-by-solids? false}}))

(let [gem-template
      (mmerge dynamic-body
              (square-hitbox 16)
              {:gem {:value 100}
               :debug-rect "#E38B2D"
               :gravity true
               :remove-when-passed? true})]
  (defn gem
    [x y]
    (-> gem-template
      (set-pos x y))))

(let [speed 400
      bomb-template
      (mmerge dynamic-body
              (square-hitbox 20)
              {:debug-rect "red"
               :remove-when-offscreen? true
               :timed-explosion {:delay 1
                                 :ticking false}
               :collision-handler (fn [self other]
                                    [[:set self [:timed-explosion :ticking] true]
                                     [:store-time self [:timed-explosion :start]]])})]
  (defn bomb
    [x y direction]
    (-> bomb-template
      (set-pos x y)
      (->/let [vx (* speed (Math/cos direction))
               vy (* speed (Math/sin direction))]
              (->/in [:vel]
                     (assoc :x vx :y vy))))))

(let [explosion-template
      (mmerge pos
              (square-hitbox 60)
              {:debug-rect "purple"
               :timed-removal {:delay 0.1}})]
  (defn explosion
    [x y start-time]
    (-> explosion-template
      (set-pos x y)
      (assoc-in [:timed-removal :start] start-time))))
