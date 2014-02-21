(ns lapwing.core
  (:require [lapwing.util :as util]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.entity.fsm :as fsm]
            [lapwing.input :as input]
            [lapwing.player :as player]
            [lapwing.game.entities :as game-entities]
            [lapwing.cameras :as cam]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->])
  (:import java.awt.event.KeyEvent)
  (:gen-class :main true))

(set! *warn-on-reflection* true)

(defn create-wall
  [x y]
  (entity/create (game-entities/wall x y)))

(defn create-entities
  []
  (entities/create
    (concat
      [(entity/create game-entities/player)]
      (for [x (range 0 800 48)]
        (create-wall x 500))
      (for [x (range 100 250 48)]
        (create-wall x 375))
      (for [y (range 0 600 48)]
        (create-wall 0 y))
      [(create-wall 500 350)])))

(defn create-canvas
  [[width height] render-state input-state]
  (let [set-key-state! (fn [state]
                         (fn [^KeyEvent e]
                           (input/set-state! input-state (.getKeyCode e) state)))
        ^java.awt.Component canvas (s/canvas 
                                     :size   [width :by height]
                                     :paint  (fn [c ^java.awt.Graphics2D g]
                                               (let [{:keys [entities time-delta camera]} @render-state]
                                                 (when entities
                                                   (doto g
                                                     (.setBackground (s.col/color "white"))
                                                     (.clearRect 0 0 width height))
                                                   (dorun
                                                     (-> entities
                                                       (entities/in-region
                                                         (cam/left camera) (cam/right camera)
                                                         (cam/top camera) (cam/bottom camera))
                                                       (entities/each
                                                         (fn [{:keys [pos debug-rect hitbox]}]
                                                           (when debug-rect
                                                             (doto g
                                                               (.setColor (s.col/color debug-rect))
                                                               (.fillRect (- (:x pos) (:x camera))
                                                                          (- (:y pos) (:y camera))
                                                                          (:width hitbox) (:height hitbox))))))))
                                                   (doto g
                                                     (.setColor (s.col/color "blue"))
                                                     (.fillRect 0 0 20 20)
                                                     (.setColor (s.col/color "white"))
                                                     (.drawString (-> time-delta / int str) 3 15)))))
                                     :listen  [:key-pressed   (set-key-state! :down)
                                               :key-released  (set-key-state! :up)])]
    (.setFocusable canvas true)
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn update-key-walkers
  [{:keys [entities input-state]}]
  (let [dx (+ (if (input/is-down? input-state :move-left)
                -1 0)
              (if (input/is-down? input-state :move-right)
                1 0))
        direction (if (neg? dx) :left :right)]
    (util/flatten-1
      (-> entities
        (entities/those-with [:key-walker :vel])
        (entities/filter #(-> % :key-walker :can-walk?))
        (entities/each
          (fn [{{:keys [speed]} :key-walker :as e}]
            (concat
              [[:accelerate e {:x (* speed dx)
                               :relative? false}]
               (when (and (not (zero? dx))
                          (entity/has-component? e :direction))
                 [:set e [:direction] direction])])))))))

(defn update-key-shooters
  [{:keys [entities input-state time]}]
  (when (input/is-down? input-state :shoot)
    (util/flatten-1
      (-> entities
        (entities/those-with [:key-shooter :pos :direction])
        (entities/each
          (fn [{{:keys [delay-start shot-delay] :or {delay-start 0}} :key-shooter :as e}]
            (when (>= (- time delay-start) shot-delay)
              [[:create (game-entities/shot (-> e :pos :x) (-> e :pos :y) (:direction e))]
               [:set e [:key-shooter :delay-start] time]])))))))

(defn move-along-dimension
  [e dim distance dir solids]
  (loop [distance distance, e e]
    (if (pos? distance)
      (let [e- (update-in e [:pos dim] dir)]
        (if-let [collison (collision/check e- solids)]
          [e collison]
          (recur (dec distance) e-)))
      [e])))

(defn move-dynamic-bodies
  [{:keys [entities time-delta]}]
  (let [solids (entities/filter entities :solid?)]
    (util/flatten-1
      (-> entities
        (entities/those-with [:pos :vel :dynamic-body])
        (entities/each
          (fn [{{vx :x vy :y} :vel {:keys [stopped-by-solids?]} :dynamic-body :as e}]
            (if stopped-by-solids?
              (let [x-dir   (if (pos? vx) inc dec)
                    y-dir   (if (pos? vy) inc dec)
                    x-step  (-> vx (* time-delta) double Math/abs Math/floor)
                    y-step  (-> vy (* time-delta) double Math/abs Math/floor)]
                (when (or (pos? x-step) (pos? y-step))
                  (let [[e x-collision] (move-along-dimension e :x x-step x-dir solids)
                        [e y-collision] (move-along-dimension e :y y-step y-dir solids)]
                    (concat
                      [[:move e (:pos e)]]
                      (when x-collision
                        (concat
                          [[:set e [:vel :x] 0]]
                          (when (entity/has-component? e :collision-handler)
                            ((:collision-handler e) e x-collision))))
                      (when y-collision
                        (concat
                          [[:set e [:vel :y] 0]]
                          (when (entity/has-component? e :collision-handler)
                            ((:collision-handler e) e y-collision))))))))
              [[:move e {:x vx
                         :y vy
                         :relative? true}]])))))))

(def gravity {:y 800})

(defn apply-gravity
  [{:keys [entities]}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:vel :gravity])
      (entities/filter :gravity)
      (entities/each
        (fn [e]
          [[:accelerate e gravity]])))))

(defn update-fsm
  [{:keys [entities] :as game-state}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:state-machine])
      (entities/each
        #(fsm/update % game-state)))))

(defn move-camera
  [{:keys [entities]}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:camera-target :pos])
      (entities/each
        (fn [e]
          [[:center-camera-on e]])))))

(defn extend-level
  [{:keys [entities camera]}]
  (let [left    (+ (:x camera) (:width camera))
        right   (+ left (:width camera))
        top     0
        bottom  600
        walls   (-> entities
                  (entities/of-type :wall)
                  (entities/in-region
                    left right top bottom)
                  entities/list)]
    (when (empty? walls)
      (for [x (range left right 48)
            :let [y 500]]
        [:create (game-entities/wall x y)]))))

(def effectors
  {:create
   (fn [{:keys [entities]} components]
     {:entities
      (entities/add entities (entity/create components))})

   :destroy
   (fn [{:keys [entities]} e]
     {:entities
      (entities/remove entities e)})

   :move
   (fn [{:keys [entities time-delta]} who {:keys [x y relative?]}]
     {:entities
      (let [update (if relative?
                     #(update-in %1 [:pos %2] + (* time-delta %3))
                     #(assoc-in %1 [:pos %2] %3))]
        (-> entities
          (entities/update-only
            who
            #(-> %
               (->/when x
                        (update :x x))
               (->/when y
                        (update :y y))))))})

   :accelerate
   (fn [{:keys [entities time-delta]} who {:keys [x y relative?]
                                           :or {relative? true}}]
     {:entities
      (let [update (if relative?
                     #(update-in %1 [:vel %2] + (* time-delta %3))
                     #(assoc-in %1 [:vel %2] %3))]
        (-> entities
          (entities/update-only
            who
            #(-> %
               (->/when x
                        (update :x x))
               (->/when y
                        (update :y y))))))})

   :set
   (fn [{:keys [entities]} who & specs]
     {:entities
      (-> entities
        (entities/update-only
          who
          #(reduce
             (fn [entity [path value]]
               (assoc-in entity path value))
             % (partition 2 specs))))})

   :update
   (fn [{:keys [entities]} who & specs]
     {:entities
      (-> entities
        (entities/update-only
          who
          #(reduce
             (fn [entity [path f]]
               (update-in entity path f))
             % (partition 2 specs))))})

   :store-time
   (fn [{:keys [entities time]} who path]
     {:entities
      (-> entities
        (entities/update-only
          who
          #(assoc-in % path time)))})
   
   :center-camera-on
   (fn [{:keys [entities camera]} who]
     {:camera
      (cam/center
        camera
        (-> entities (entities/get who) :pos))})})

(defn effect-statements
  [game-state statements] 
  (reduce
    (fn [game-state [statement-type & data]]
      (if-let [effector (get effectors statement-type)]
        (merge game-state (apply effector game-state data))
        (throw (Exception. (str "No effector for " statement-type)))))
    game-state
    (filter identity statements)))

(defn now
  []
  (/ (System/nanoTime) 1000000000))

(defn run
  [render-state input-state]
  (loop [game-state {:entities  (create-entities)
                     :camera    (cam/simple-camera 800 600)
                     :time      (now)}]
    (let [start-time  (now)
          time-delta    (- start-time (:time game-state))
          game-state    (assoc game-state
                               :time start-time
                               :time-delta time-delta
                               :input-state (input/update! input-state))
          es          (:entities game-state)
          game-state  (reduce
                        (fn [game-state produce]
                          (effect-statements game-state
                                             (produce game-state)))
                        game-state
                        [update-key-walkers
                         update-key-shooters
                         apply-gravity
                         update-fsm
                         move-dynamic-bodies
                         move-camera
                         extend-level])]
      (send render-state (constantly game-state))
      ; eat up the remaning time
      (let [remaining-time (- 1/30
                              (- (now) start-time))]
        (when (pos? remaining-time)
          (Thread/sleep (* 1000 remaining-time))))
      (recur game-state))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        input-state   (doto (input/create-state)
                        (input/def!
                          :jump       KeyEvent/VK_X
                          :shoot      KeyEvent/VK_C
                          :move-left  [KeyEvent/VK_KP_LEFT  KeyEvent/VK_LEFT]
                          :move-right [KeyEvent/VK_KP_RIGHT KeyEvent/VK_RIGHT]
                          :move-down  [KeyEvent/VK_KP_DOWN  KeyEvent/VK_DOWN]))
        canvas        (create-canvas [800 600] render-state input-state)]
    (doto (Thread. #(run render-state input-state))
      .start)
    (s/invoke-later
      (-> (s/frame
            :title    "Lapwing"
            :content  canvas
            :on-close :exit)
        s/pack!
        s/show!))))
