(ns lapwing.image
  (:refer-clojure :exclude [get])
  (:import javax.imageio.ImageIO
           java.io.File))

(def aliases (atom {}))

(def loaded-images (atom {}))

(defn def!
  [& {:as mappings}]
  (doseq [[from to] mappings]
    (swap! aliases assoc from to)))

(defn canonical-name
  [name]
  (if (contains? @aliases name)
    (clojure.core/get @aliases name)
    name))

(defn loaded?
  [name]
  (contains? @loaded-images name))

(defn load!
  [name]
  (swap! loaded-images
         assoc name (-> name File. ImageIO/read)))

(defn get
  [name]
  (let [name (canonical-name name)]
    (when-not (loaded? name)
      (load! name))
    (clojure.core/get @loaded-images name)))
