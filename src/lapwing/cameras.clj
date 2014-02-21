(ns lapwing.cameras
  (require [lapwing.entity :as entity]))

(defn half-width
  [camera]
  (/ (:width camera) 2))

(defn half-height
  [camera]
  (/ (:height camera) 2))

(defn move-to
  [camera {:keys [x y]}]
  (assoc camera :x x :y y))

(defn center
  [camera {center-x :x center-y :y}]
  (move-to camera
           {:x (- center-x (half-width camera))
            :y (- center-y (half-height camera))}))

(defn simple-camera
  [width height]
  {:x 0
   :y 0
   :width width
   :height height})
