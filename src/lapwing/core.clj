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
         :key-walker {:speed 7, :can-walk true}
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
  [es input-state]
  (let [dx (+ (if (input/is-down? input-state :move-left)
                -1 0)
              (if (input/is-down? input-state :move-right)
                1 0))]
    (-> es
      (entities/update-those-with
        [:key-walker :vel]
        (fn [{{:keys [speed can-walk] :or {speed 1}} :key-walker :as e}]
          (-> e
            (->/when can-walk
                     (assoc-in [:vel :x] (* speed dx)))))))))

(defn maybe-move-step
  [e dim step dir solids]
  (if (pos? step)
    (let [e- (update-in e [:pos dim] dir)]
      (if-not (collision/? e- solids)
        [e- (dec step)]
        [(assoc-in e [:vel dim] 0) 0]))
    [e 0]))

(defn move-dynamic-bodies
  [es]
  (let [solids (entities/filter es :solid?)]
    (-> es
      (entities/update-those-with
        [:pos :vel :dynamic-body]
        (fn [{{vx :x vy :y} :vel {:keys [stopped-by-solids?]} :dynamic-body :as e}]
          (if stopped-by-solids?
            (let [x-dir (if (pos? vx) inc dec)
                  y-dir (if (pos? vy) inc dec)]
              (loop [x-step (Math/floor (Math/abs vx)), y-step (Math/floor (Math/abs vy)), e e]
                (if (or (pos? x-step) (pos? y-step))
                  (let [[e x-step]  (maybe-move-step e :x x-step x-dir solids)
                        [e y-step]  (maybe-move-step e :y y-step y-dir solids)]
                    (recur x-step y-step e))
                  e)))
            (-> e
              (->/in [:pos]
                     (update-in [:x] + vx)
                     (update-in [:y] + vy)))))))))

(def gravity {:y 1})

(defn apply-gravity
  [es]
  (util/flatten-1
    (entities/each
      (-> es
        (entities/those-with [:vel :gravity])
        (entities/filter :gravity))
      (return [[:accelerate % gravity]]))))

(defn update-fsm
  [es input-state]
  (util/flatten-1
    (entities/each
      (entities/those-with es [:state-machine])
      (return [[:update-entity % (fn [e es input-state]
                                   (fsm/update e es input-state))]]))))

(defn run
  [render-state input-state]
  (loop [game-state {:entities (create-entities)}]
    (let [now (java.util.Date.)
          input-state (input/update! input-state)
          es          (-> (:entities game-state)
                        (updated-key-walkers input-state))
          statements  (reduce
                        (fn [statements producer]
                          (concat statements (producer)))
                        [] [#(apply-gravity es)
                            #(update-fsm es input-state)])
          es          (reduce
                        (fn [es statement]
                          (case (first statement)
                            :accelerate
                            (let [[_ who {:keys [x y]}] statement]
                              (entities/update-only es who
                                                    #(-> %
                                                       (->/when x
                                                                (update-in [:vel :x] + x))
                                                       (->/when y
                                                                (update-in [:vel :y] + y)))))

                            :update-entity
                            (let [[_ who updater] statement]
                              (entities/update-only es who
                                                    #(updater % es input-state)))))
                        es statements)
          es          (move-dynamic-bodies es)
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
