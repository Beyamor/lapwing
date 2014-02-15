(ns lapwing.core
  (:require [lapwing.util :as util]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->])
  (:import java.awt.event.KeyEvent))

(defn create-entities
  []
  (entities/create
    (concat
      [(entity/create
         :pos
         {:x 300
          :y 300}
         :keyboard-movement
         {:speed 7}
         :debug-rect
         {:width   48
          :height  48
          :color   :red})]
      (for [x (range 0 800 48)]
        (entity/create
          :pos
          {:x x
           :y 500}
          :debug-rect
          {:width   48
           :height  48
           :color   :black})))))

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
                               (doseq [[_ {:keys [pos debug-rect]}] (:entities render-state)
                                       :when debug-rect]
                                 (doto g
                                   (.setColor (s.col/color (:color debug-rect)))
                                   (.fillRect (:x pos) (:y pos)
                                              (:width debug-rect) (:height debug-rect)))))))
                 :listen  [:key-pressed   (set-key-state! :down)
                           :key-released  (set-key-state! :up)])]
    (.setFocusable canvas true)
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn input-direction-deltas
  [input-state]
  (let [dx  (+ (if (input/is-down? input-state :walk-left)
                 -1 0)
               (if (input/is-down? input-state :walk-right)
                 1 0))
        dy  (+ (if (input/is-down? input-state :walk-up)
                 -1 0)
               (if (input/is-down? input-state :walk-down)
                 1 0))]
    (if (and (not= dx 0) (not= dy 0))
      [(* dx (Math/sqrt 1/2)) (* dy (Math/sqrt 1/2))]
      [dx dy])))

(defn move-player
  [es input-state]
  (let [[dx dy] (input-direction-deltas input-state)]
    (-> es
      (entities/update-those-with
        [:keyboard-movement :pos]
        (fn [e]
          (let [speed (get-in e [:keyboard-movement :speed] 1)]
            (-> e
              (->/in [:pos]
                     (update-in [:x] + (* speed dx))
                     (update-in [:y] + (* speed dy))))))))))

(defn run
  [render-state input-state]
  (loop [game-state {:entities (create-entities)}]
    (let [input-state @input-state
          new-state   (-> game-state
                        (->/in [:entities]
                               (move-player input-state)))]
      (send render-state (constantly new-state))
      (Thread/sleep 20)
      (recur new-state))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        input-state   (doto (input/create-state)
                        (input/def!
                          :walk-left  [KeyEvent/VK_A KeyEvent/VK_KP_LEFT KeyEvent/VK_LEFT]
                          :walk-right [KeyEvent/VK_D KeyEvent/VK_KP_RIGHT KeyEvent/VK_RIGHT]
                          :walk-up    [KeyEvent/VK_W KeyEvent/VK_KP_UP KeyEvent/VK_UP]
                          :walk-down  [KeyEvent/VK_S KeyEvent/VK_KP_UP KeyEvent/VK_DOWN]))
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
