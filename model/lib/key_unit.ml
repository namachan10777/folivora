module M = Scad_ml.Model
open Scad_ml.Util

let keycap_chery_r2 =
    ( (17.7, 17.9, 4.0)
    , M.hull
        [ M.cube (11.5, 0.1, 0.1) |>> ((17.7 -. 11.5) /. 2., 17.3, 6.8)
        ; M.cube (11.5, 0.1, 0.1) |>> ((17.7 -. 11.5) /. 2., 3.3, 7.3)
        ; M.cube (17.7, 17.9, 0.1) ] )

let keycap_chery_r3 =
    ( (17.7, 17.9, 4.0)
    , M.hull
        [ M.cube (11.5, 0.1, 0.1) |>> ((17.7 -. 11.5) /. 2., 17.3, 5.5)
        ; M.cube (11.5, 0.1, 0.1) |>> ((17.7 -. 11.5) /. 2., 3.3, 7.0)
        ; M.cube (17.7, 17.9, 0.1) ] )

let keycap_chery_r4 =
    ( (17.7, 17.9, 4.0)
    , M.hull
        [ M.cube (11.5, 0.1, 0.1) |>> ((17.7 -. 11.5) /. 2., 17.3, 5.5)
        ; M.cube (11.5, 0.1, 0.1) |>> ((17.7 -. 11.5) /. 2., 3.3, 8.3)
        ; M.cube (17.7, 17.9, 0.1) ] )

let keycap_choc =
    ( (17.7, 17.9, 4.0)
    , M.hull
        [ M.cube (16.5, 15.5, 0.1)
          |>> ((17.7 -. 16.5) /. 2., (16.5 -. 17.5) /. 2., 3.8)
        ; M.cube (17.5, 16.5, 0.1) ] )

let centering_d (dx, dy) (w, d) (w', d') =
    (((w -. w') /. 2.) +. dx, ((d -. d') /. 2.) +. dy, 0.0)

type cap_t = (float * float * float) * M.t

let kailh_box (dx, dy) (w, d, h) cap =
    let lump = M.cube (w, d, h) in
    let cap =
        match cap with
        | Some ((cap_w, cap_d, cap_offset), body) ->
            [ body
              |>> ( centering_d (dx, dy) (w, d) (cap_w, cap_d)
                  <+> (0., 0., cap_offset +. h) ) ]
        | None -> []
    in
    M.difference
      (M.union
         ( M.difference lump
             [ M.cube (16., 16., 1.001)
               |>> (((w -. 16.) /. 2.) +. dx, ((d -. 16.) /. 2.) +. dy, h -. 1.)
             ; M.cube (6., 16., h -. 1.0 -. 1.5 +. 0.001)
               |>> (((w -. 6.) /. 2.) +. dx, ((d -. 16.) /. 2.) +. dy, -0.001)
             ]
         :: ( M.cube (16., 10., 1.)
            |>> (((w -. 16.) /. 2.) +. dx, ((d -. 10.) /. 2.) +. dy, h -. 1.) )
         :: cap ))
      [ M.cube (14., 14., h +. 0.02)
        |>> (((w -. 14.) /. 2.) +. dx, ((d -. 14.) /. 2.) +. dy, -0.01) ]

let kailh_choc (dx, dy) (w, d, h) cap =
    let lump = M.cube (w, d, h) in
    let cap =
        match cap with
        | Some ((cap_w, cap_d, cap_offset), body) ->
            [ body
              |>> ( centering_d (dx, dy) (w, d) (cap_w, cap_d)
                  <+> (0., 0., cap_offset +. h) ) ]
        | None -> []
    in
    let cut =
        M.difference lump
          [ M.cube (14., 14., h +. 0.02)
            |>> (((w -. 14.) /. 2.) +. dx, ((d -. 14.) /. 2.) +. dy, -0.01)
          ; M.cube (16., 16., 1.01)
            |>> (((w -. 16.) /. 2.) +. dx, ((d -. 16.) /. 2.) +. dy, h -. 1.)
          ; M.cube (16., 14., h -. 1.0 -. 1.3 +. 0.001)
            |>> (((w -. 16.) /. 2.) +. dx, ((d -. 16.) /. 2.) +. dy, -0.001) ]
    in
    M.union
      [ cut
      ; M.difference
          (M.union
             ( ( M.cube (16., 10., 1.)
               |>> (((w -. 16.) /. 2.) +. dx, ((d -. 10.) /. 2.) +. dy, h -. 1.)
               )
             :: ( M.cube (16., 3., h)
                |>> (((w -. 16.) /. 2.) +. dx, ((d -. 3.) /. 2.) +. dy, 0.) )
             :: cap ))
          [ M.cube (14., 14., h +. 0.02)
            |>> (((w -. 14.) /. 2.) +. dx, ((d -. 14.) /. 2.) +. dy, -0.01) ] ]

let bittradeone_trackball (dx, dy) (w, d, h) =
    let lump = M.cube (w, d, h +. 6.) in
    M.difference lump
      [ M.cube (17., 17., 6. +. 0.001)
        |>> (((w -. 17.) /. 2.) +. dx, ((d -. 17.) /. 2.) +. dy, h)
      ; M.cube (6.35, 1.27, h +. 6. +. 0.02)
        |>> ( ((w -. 17.) /. 2.) +. dx +. 7.5
            , ((d -. 17.) /. 2.) +. dy +. 1.0
            , -0.01 ) ]

let dummy s _ = M.cube s
