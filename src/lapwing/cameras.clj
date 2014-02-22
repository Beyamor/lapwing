(ns lapwing.cameras
  (require [lapwing.entity :as entity]))

(defn half-width
  [camera]
  (/ (:width camera) 2))

(defn half-height
  [camera]
  (/ (:height camera) 2))

(defn move-to
  [camera x y]
  (assoc camera :x x :y y))

(defn center-x
  [camera x]
  (assoc camera :x (- x (half-width camera))))

(defn center-y
  [camera y]
  (assoc camera :y (- y (half-height camera))))

(defn center
  [camera x y]
  (-> camera
    (center-x x)
    (center-y y)))

(defn left
  [camera]
  (:x camera))

(defn right
  [camera]
  (+ (left camera) (:width camera)))

(defn top
  [camera]
  (:y camera))

(defn bottom
  [camera]
  (+ (top camera) (:height camera)))

(defn simple-camera
  [width height]
  {:x 0
   :y 0
   :width width
   :height height})
