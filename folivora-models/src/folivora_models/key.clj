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

(defn key-elm-pos [times]
  (defn f [times tilt]
    (if (> times 0)
      (let [p (util/add [0
                         (* (nth key-size 2) (Math/sin tilt))
                         (- (nth key-size 2) (* (nth key-size 2) (Math/cos tilt)))]
                        [0
                         (* (nth key-size 1) (Math/cos (- tilt key-elm-tilt)))
                         (* (nth key-size 1) (Math/sin (- tilt key-elm-tilt)))])]
        (util/add p (f (- times 1) (+ tilt key-elm-tilt))))
      [0 0 0]))
    (f times key-elm-tilt))

(defn key-elm-peak [times]
  (util/add (key-elm-pos times)
            [0
             (* (nth key-size 1) (Math/cos (* key-elm-tilt times)))
             (* (nth key-size 1) (Math/sin (* key-elm-tilt times)))]))

; FIXME naming
(defn key-col-mt [times]
  (util/add (key-elm-peak times)
            [0
             (* (nth key-size 2) (Math/sin (* -1 key-elm-tilt times)))
             (* (nth key-size 2) (Math/cos (* -1 key-elm-tilt times)))]))

(defn key-joint-padding [times]
  (let [gap (util/mul [1 0 0] key-size)]
    (model/polyhedron
      [(key-elm-peak times)
       (key-elm-pos (+ times 1))
       (key-col-mt times)
       (util/add (key-elm-peak times) gap)
       (util/add (key-elm-pos (+ times 1)) gap)
       (util/add (key-col-mt times) gap)]
      [[0 1 2]
       [3 4 1 0]
       [5 4 3]
       [4 5 2 1]])))

(def key-col-inner
  (model/union
    (->> key-elm)
    (->> key-elm
         (model/rotate key-elm-tilt [1 0 0])
         (model/translate (key-elm-pos 1)))
    (key-joint-padding 0)))
