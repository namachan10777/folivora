(ns folivora-models.key
  (:require [scad-clj.scad :as scad]
            [scad-clj.model :as model]
            [unicode-math.core :refer :all]
            [folivora-models.util :as util]))

; user defined constants
(def key-size [19 21 5])
(def entrane-hole-size [14 14 5])
(def hole-size [15 15 3])
(def key-elm-tilt (/ π 12))

(defn center-move-vec [cube1 cube2]
  (map (fn [p1 p2] (/ (- p1 p2) 2)) cube1 cube2))

(def key-elm
  (model/difference
    (->> (util/cube key-size))
    (->> (util/cube entrane-hole-size)
         (model/translate (->> (center-move-vec key-size entrane-hole-size)
                               (util/mul [1 1 (nth hole-size 2)]))))
    (->> (util/cube hole-size)
         (model/translate (->> (center-move-vec key-size hole-size)
                               (util/mul [1 1 0]))))))

(defn key-pos-delta-base [tilt]
  [0
   (*  1 (nth key-size 2) (Math/sin tilt))
   (* -1 (nth key-size 2) (Math/cos tilt))])

(defn key-pos-delta-tip [tilt]
  [0
   (* (nth key-size 1) (Math/cos tilt))
   (* (nth key-size 1) (Math/sin tilt))])

(defn key-pos-delta-joint [tilt]
  [0
   (* -1 (nth key-size 2) (Math/sin tilt))
   (*  1 (nth key-size 2) (Math/cos tilt))])

(defn col-joint-padding [tilt]
  (let [w (nth key-size 0)
        right [[0 0 0]
               (key-pos-delta-joint 0)
               (util/add (key-pos-delta-joint 0) (key-pos-delta-base tilt))]]
    (model/polyhedron
      (concat right (map (fn [p] (util/add [w 0 0] p)) right))
      [[0 2 1]
       [0 1 4 3]
       [1 2 5 4]
       [3 5 2 0]
       [3 4 5]
       ])))

(defn key-col [near far]
  (defn make-far [times base-pos tilt]
    (let [current-key-elm
          (->> key-elm
               (model/rotate tilt [1 0 0])
               (model/translate base-pos))
          current-key-padding
          (->> (col-joint-padding key-elm-tilt)
               (model/translate [0 (nth key-size 1) 0])
               (model/rotate tilt [1 0 0])
               (model/translate base-pos))]
      (if (<= times 1)
        current-key-elm
        (model/union
          current-key-elm
          current-key-padding
          (make-far
            (- times 1)
            (util/add
              base-pos
              (key-pos-delta-tip tilt)
              (key-pos-delta-joint tilt)
              (key-pos-delta-base (+ tilt key-elm-tilt)))
            (+ tilt key-elm-tilt))))))
  (defn make-near [times]
    (->> (model/union
           (make-far times
                     (util/add
                       (key-pos-delta-base key-elm-tilt)
                       (key-pos-delta-joint 0))
                     key-elm-tilt)
           (col-joint-padding key-elm-tilt))
         (model/mirror [0 1 0])))
  (model/union
    (make-far (+ far 1) [0 0 0] 0)
    (make-near near)))
