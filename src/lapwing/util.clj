(ns lapwing.util
  (:require [clojure.walk :as w]))

(def ids (atom 0))

(defn gen-id
  []
  (swap! ids inc))

(defn map-over-keys
  [f m]
  (into m
        (for [[k v] m]
          [k (f v)])))

(defn flatten-1
  [s]
  (apply concat s))

(defn anon-arg?
  [thing]
  (and (symbol? thing)
       (.startsWith (name thing) "%")))

(defn anon-arg->number
  [sym]
  (if (= "%" (name sym))
    1
    (-> sym name (subs 1) Integer/parseInt)))

(defn get-number-of-anon-args
  [form]
  (let [number-of-args (atom 0)]
    (w/postwalk
      (fn [thing]
        (when (anon-arg? thing)
          (swap! number-of-args max (anon-arg->number thing)))
        thing)
      form)
    @number-of-args))

(defmacro return
  [form]
  (let [number-of-args  (get-number-of-anon-args form)
        args            (repeatedly number-of-args gensym)]
    `(fn [~@args]
       ~(w/postwalk
          (fn [thing]
            (if (anon-arg? thing)
              (nth args (dec (anon-arg->number thing)))
              thing))
          form))))
