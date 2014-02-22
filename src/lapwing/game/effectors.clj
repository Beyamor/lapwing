(ns lapwing.game.effectors
  (:require [lapwing.util]
            [lapwing.entities :as entities]
            [lapwing.entity :as entity]
            [lapwing.input :as input]
            [lapwing.cameras :as cam]
            [lonocloud.synthread :as ->]))

(def all
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
      (-> camera
        (cam/center-x
          (-> entities (entities/get who) :pos :x)))})
   
   :section-added
   (fn [{:keys [last-section]}]
     {:last-section
      (inc last-section)})})
