(ns lapwing.core
  (:require [lapwing.util :as util :refer [return]]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entities.collisions :as collision]
            [lapwing.entity :as entity]
            [lapwing.entity.fsm :as fsm]
            [lapwing.input :as input]
            [lapwing.player :as player]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->])
  (:import java.awt.event.KeyEvent))

(defn create-wall
  [x y]
  (entity/create
    :pos {:x x
          :y y}
    :debug-rect "black"
    :hitbox {:width   48
             :height  48}
    :solid? true))

(defn create-entities
  []
  (entities/create
    (concat
      [(entity/create
         :pos {:x 300
               :y 300}
         :vel {:x 0
               :y 0}
         :key-walker {:speed 7, :can-walk? true}
         :debug-rect "red"
         :gravity true
         :hitbox {:width  48
                  :height 48}
         :state-machine {:name   :player
                         :state  :falling}
         :player-jumper {:initial-amount         10
                         :additional-amount      0.5
                         :number-of-additionals  5}
         :dynamic-body
         {:stopped-by-solids? true})]
      (for [x (range 0 800 48)]
        (create-wall x 500))
      (for [x (range 100 250 48)]
        (create-wall x 375))
      (for [y (range 0 600 48)
            x [0 (- 800 48)]]
        (create-wall x y))
      [(create-wall 500 350)])))

(defn create-canvas
  [[width height] render-state input-state]
  (let [set-key-state! (fn [state]
                         (fn [e]
                           (input/set-state! input-state (.getKeyCode e) state)))
        canvas (s/canvas 
                 :size   [width :by height]
                 :paint  (fn [c g]
                           (let [render-state @render-state]
                             (when render-state
                               (doto g
                                 (.setBackground (s.col/color "white"))
                                 (.clearRect 0 0 width height))
                               (doseq [[_ {:keys [pos debug-rect hitbox]}] (:entities render-state)
                                       :when debug-rect]
                                 (doto g
                                   (.setColor (s.col/color debug-rect))
                                   (.fillRect (:x pos) (:y pos)
                                              (:width hitbox) (:height hitbox)))))))
                 :listen  [:key-pressed   (set-key-state! :down)
                           :key-released  (set-key-state! :up)])]
    (.setFocusable canvas true)
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn updated-key-walkers
  [{:keys [entities input-state]}]
  (let [dx (+ (if (input/is-down? input-state :move-left)
                -1 0)
              (if (input/is-down? input-state :move-right)
                1 0))]
    (util/flatten-1
      (-> entities
        (entities/those-with [:key-walker :vel])
        (entities/filter #(-> % :key-walker :can-walk?))
        (entities/each
          (fn [{{:keys [speed]} :key-walker :as e}]
            [[:accelerate e {:x (* speed dx)
                             :relative? false}]]))))))

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
  [{:keys [entities]}]
  (let [solids (entities/filter entities :solid?)]
    (util/flatten-1
      (-> entities
        (entities/those-with [:pos :vel :dynamic-body])
        (entities/each
          (fn [{{vx :x vy :y} :vel {:keys [stopped-by-solids?]} :dynamic-body :as e}]
            (if stopped-by-solids?
              (let [x-dir   (if (pos? vx) inc dec)
                    y-dir   (if (pos? vy) inc dec)
                    x-step  (Math/floor (Math/abs vx))
                    y-step  (Math/floor (Math/abs vy))]
                (when (or (pos? x-step) (pos? y-step))
                  (let [[e x-collision] (move-along-dimension e :x x-step x-dir solids)
                        [e y-collision] (move-along-dimension e :y y-step y-dir solids)]
                    (concat
                      [[:move e (:pos e)]]
                      (when x-collision
                        [[:set e [:vel :x] 0]])
                      (when y-collision
                        [[:set e [:vel :y] 0]])))))
              [[:move e {:x vx
                         :y vy
                         :relative? true}]])))))))

(def gravity {:y 1})

(defn apply-gravity
  [{:keys [entities]}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:vel :gravity])
      (entities/filter :gravity)
      (entities/each
        (return [[:accelerate % gravity]])))))

(defn update-fsm
  [{:keys [entities] :as game-state}]
  (util/flatten-1
    (-> entities
      (entities/those-with [:state-machine])
      (entities/each
        #(fsm/update % game-state)))))

(def effectors
  {:move
   (fn [{:keys [entities]} who {:keys [x y relative?]}]
     (let [update (if relative?
                    #(update-in %1 [:pos %2] + %3)
                    #(assoc-in %1 [:pos %2] %3))]
       (-> entities
         (entities/update-only
           who
           #(-> %
              (->/when x
                       (update :x x))
              (->/when y
                       (update :y y)))))))
   :accelerate
   (fn [{:keys [entities]} who {:keys [x y relative?]
                                :or {relative? true}}]
     (let [update (if relative?
                    #(update-in %1 [:vel %2] + %3)
                    #(assoc-in %1 [:vel %2] %3))]
       (-> entities
         (entities/update-only
           who
           #(-> %
              (->/when x
                       (update :x x))
              (->/when y
                       (update :y y)))))))

   :set
   (fn [{:keys [entities]} who & specs]
     (-> entities
       (entities/update-only
         who
         #(reduce
            (fn [entity [path value]]
              (assoc-in entity path value))
            % (partition 2 specs)))))

   :update
   (fn [{:keys [entities]} who & specs]
     (-> entities
       (entities/update-only
         who
         #(reduce
            (fn [entity [path f]]
              (update-in entity path f))
            % (partition 2 specs)))))})

(defn effect-statements
  [game-state statements] 
  (:entities
    (reduce
      (fn [game-state [statement-type & data]]
        (if-let [effector (get effectors statement-type)]
          (assoc game-state :entities (apply effector game-state data))
          (throw (Exception. (str "No effector for " statement-type)))))
      game-state statements)))

(defn run
  [render-state input-state]
  (loop [game-state {:entities (create-entities)}]
    (let [now (java.util.Date.)
          input-state (input/update! input-state)
          es          (:entities game-state)
          es          (reduce
                        (fn [es produce]
                          (let [game-state {:entities es :input-state input-state}]
                            (effect-statements game-state
                                               (produce game-state))))
                        es
                        [updated-key-walkers
                         apply-gravity
                         update-fsm
                         move-dynamic-bodies])
          new-state   (assoc game-state :entities es)]
      (send render-state (constantly new-state))
      (Thread/sleep 20)
      (recur new-state))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        input-state   (doto (input/create-state)
                        (input/def!
                          :jump       KeyEvent/VK_X
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
