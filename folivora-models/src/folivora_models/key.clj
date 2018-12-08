(ns folivora-models.key
  (:require [scad-clj.scad :as scad]
            [scad-clj.model :as model]
            [unicode-math.core :refer :all]
            [folivora-models.util :as util]))

; user defined constants
(def key-size [19 21 5])
(def entrane-hole-size [14 14 5])
(def hole-size [15 15 3])

(defn center-move-vec [cube1 cube2]
  [(/ (- (nth cube1 0) (nth cube2 0)) 2)
   (/ (- (nth cube1 1) (nth cube2 1)) 2)
   (/ (- (nth cube1 2) (nth cube2 2)) 2)])

(def key-elm
  (model/difference
    (->> (util/cube key-size))
    (->> (util/cube entrane-hole-size)
         (model/translate (->> (center-move-vec key-size entrane-hole-size)
                               (util/mul [1 1 (nth hole-size 2)]))))
    (->> (util/cube hole-size)
         (model/translate (->> (center-move-vec key-size hole-size)
                               (util/mul [1 1 0]))))))
