module M = Scad_ml.Model
open Core
open Scad_ml.Util

let screwes = [(16.51, 55.245); (38.1, 52.07); (20.955, 2.54); (38.1, 2.54)]

let w = 40.64

let d = 64.135

let t = 4.

let pcb_t = 1.8

let plate_t = 2.6

let space_t = 2.0

let rj45 =
    M.cube (18.451, 16.51, 13.72) |>> (0., 0., plate_t +. space_t +. pcb_t)

let wire_hole =
    M.cube (40.64 -. 23., 45.72 -. 31.75, 50.)
    |>> (23., 31.75, plate_t +. space_t +. pcb_t)

let top =
    M.union
      [ M.difference
          ( M.cube (w -. 13.335, d, t)
          |>> (13.335, 0., pcb_t +. plate_t +. (2. *. space_t)) )
          [ M.cube (0.676, 0.676, t +. 0.02)
            |>> (13.334, d -. 0.675, pcb_t +. plate_t +. (2. *. space_t) -. 0.01)
          ]
      ; M.cylinder 0.675 t ~fn:30
        |>> (13.335 +. 0.675, d -. 0.675, pcb_t +. plate_t +. (2. *. space_t))
      ]

let hollow =
    M.union
      ( [ M.cube (w, d, pcb_t +. plate_t +. (2. *. space_t) +. 0.001)
          |>> (0., 0., -0.001)
        ; rj45
        ; wire_hole ]
      @ List.map screwes ~f:(fun (x, y) ->
            M.cylinder 1.6 3.3 ~fn:30
            |>> (x, y, pcb_t +. plate_t +. (2. *. space_t) -. 0.001)) )
