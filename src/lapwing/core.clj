(ns lapwing.core
  (:require [lapwing.util :as util]
            [lapwing.image :as image]
            [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [seesaw.core :as s]
            [seesaw.color :as s.col]
            [seesaw.timer :as s.time]
            [lonocloud.synthread :as ->]))

(defn create-entities
  []
  (entities/create
    (concat
      [(entity/create
         :pos        {:x 300
                      :y 300}
         :debug-rect {:width   48
                      :height  48
                      :color   :red})]
      (for [x (range 0 800 48)]
        (entity/create
          :pos          {:x x
                         :y 500}
          :debug-rect {:width   48
                       :height  48
                       :color   :black})))))

(defn create-canvas
  [[width height] render-state]
  (let [canvas (s/canvas 
                 :size   [width :by height]
                 :paint  (fn [c g]
                           (let [render-state @render-state]
                             (when render-state
                               (doto g
                                 (.setBackground (s.col/color "white"))
                                 (.clearRect 0 0 width height)
                                 (.setColor (s.col/color "black")))
                               (doseq [[_ {:keys [pos debug-rect]}] (:entities render-state)
                                       :when debug-rect]
                                 (doto g
                                   (.setColor (s.col/color (:color debug-rect)))
                                   (.fillRect (:x pos) (:y pos)
                                              (:width debug-rect) (:height debug-rect))))))))]
    (s.time/timer
      (fn [_]
        (s/repaint! canvas))
      :delay 17)
    canvas))

(defn update-positions
  [entities]
  (util/map-over-keys
    (fn [e]
      (update-in e [:pos :x] + 1))
    entities))

(defn run
  [render-state]
  (try
    (loop [game-state {:entities (create-entities)}]
      (let [new-state (-> game-state
                        (->/in [:entities]
                               update-positions))]
        (send render-state (constantly new-state))
        (Thread/sleep 20)
        (recur new-state)))
    (catch Exception e
      (.printStackTrace e))))

(defn -main
  [& args]
  (let [render-state  (agent nil)
        canvas        (create-canvas [800 600] render-state)]
    (doto (Thread. #(run render-state))
      .start)
    (s/invoke-later
      (-> (s/frame
            :title    "Lapwing"
            :content  canvas
            :on-close :exit)
        s/pack!
        s/show!))))
