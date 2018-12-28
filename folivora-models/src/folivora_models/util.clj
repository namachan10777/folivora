(ns folivora-models.util
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :as scad]
            [scad-clj.model :as model]
            [unicode-math.core :refer :all]))

(defn cube [size]
  (model/cube (nth size 0) (nth size 1) (nth size 2) :center false))

(defn inv [arg]
  (map - arg))

(defn vecop [op args]
  (defn f [v1 v2]
    (map op v1 v2))
  (vec (reduce f args)))

(defn mul [arg & args]
  (vecop * (cons arg args)))

(defn add [arg & args]
  (vecop + (cons arg args)))

(defn sub [arg & args]
  (vecop + (cons arg (map inv args))))
