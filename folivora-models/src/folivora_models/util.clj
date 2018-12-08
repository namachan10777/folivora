(ns folivora-models.util
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :as scad]
            [scad-clj.model :as model]
            [unicode-math.core :refer :all]))

(defn cube [size]
  (model/cube (nth size 0) (nth size 1) (nth size 2) :center false))

(defn mul [rhs lhs]
  [(* (nth rhs 0) (nth lhs 0))
   (* (nth rhs 1) (nth lhs 1))
   (* (nth rhs 2) (nth lhs 2))
   ])

(defn add [rhs lhs]
  [(+ (nth rhs 0) (nth lhs 0))
   (+ (nth rhs 1) (nth lhs 1))
   (+ (nth rhs 2) (nth lhs 2))
   ])

(defn inv [opr]
  [(- (nth opr 0)) (- (nth opr 1)) (- (nth opr 2))])
