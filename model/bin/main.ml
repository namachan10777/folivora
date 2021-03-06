let build scad filename =
    let oc =
        open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename
    in
    Scad_ml.Util.write oc scad ; close_out oc

let key_size = (19.0, 19.0, 5.0)

let kailh_box = Model.Key_unit.kailh_box

let kailh_choc = Model.Key_unit.kailh_choc

let pi = Scad_ml.Core.pi

module P = Model.Pad
module M = Scad_ml.Model
module K = Model.Key_unit
open Core
open Scad_ml.Util

let cylinder_top = M.cylinder 3.0 3.0 ~center:true

let cylinder_bottom = M.cylinder 3.0 3.0 ~center:true |>> (0., 0., -3.)

let k11 =
    { P.a= (-.pi /. 10., (pi /. 20.) +. (pi /. 20.), pi /. 30.)
    ; P.f= kailh_box (1.5, 1.5)
    ; P.cap= Some K.keycap_chery_r4
    ; P.p= (-3.5, -30., 32.0)
    ; P.size= (20., 20., 6.) }

let k12 =
    { P.a= (pi /. 20., (pi /. 12.) +. (pi /. 20.), pi /. 30.)
    ; P.f= kailh_box (1.5, 0.)
    ; P.cap= Some K.keycap_chery_r3
    ; P.p= (-10., -4.5, 26.5)
    ; P.size= (20., 17., 6.) }

let k13 =
    { P.a= (pi /. 6., (pi /. 12.) +. (pi /. 20.), pi /. 20.)
    ; P.f= kailh_box (1.5, 0.)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (-9., 18.5, 29.5)
    ; P.size= (20., 17., 6.) }

let k21 =
    { P.a= (-.pi /. 10., pi /. 20., pi /. 60.)
    ; P.f= kailh_box (0., 1.5)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (18., -26., 23.0)
    ; P.size= (17., 20., 6.) }

let k22 =
    { P.a= (pi /. 20., pi /. 20., pi /. 60.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r3
    ; P.p= (14., 0., 17.)
    ; P.size= (17., 17., 6.) }

let k23 =
    { P.a= (pi /. 8., pi /. 20., pi /. 60.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (14.5, 21.0, 20.0)
    ; P.size= (17., 17., 6.) }

let kp_p = (45., -64., 29.0)

let kp =
    { P.a= (pi /. 10., pi /. 20., 0.)
    ; P.cap= Some K.keycap_choc
    ; P.f= kailh_choc (0., 0.)
    ; P.p= kp_p
    ; P.size= (24., 24., 3.5) }

let k31 =
    { P.a= (-.pi /. 10., pi /. 20., 0.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (36., -18., 17.)
    ; P.size= (17., 17., 6.) }

let k32 =
    { P.a= (pi /. 20., pi /. 20., 0.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r3
    ; P.p= (35., 6., 11.)
    ; P.size= (17., 17., 6.) }

let k33 =
    { P.a= (pi /. 8., pi /. 20., 0.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (36., 27., 15.)
    ; P.size= (17., 17., 6.) }

let k41 =
    { P.a= (-.pi /. 10., pi /. 20., -.pi /. 60.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (56., -25., 17.)
    ; P.size= (17., 17., 6.) }

let k42 =
    { P.a= (pi /. 20., pi /. 20., -.pi /. 60.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r3
    ; P.p= (56., -1., 9.)
    ; P.size= (17., 17., 6.) }

let k43 =
    { P.a= (pi /. 8., pi /. 20., -.pi /. 60.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (58., 22., 13.)
    ; P.size= (17., 17., 6.) }

let k51 =
    { P.a= (-.pi /. 10., pi /. 20., -.pi /. 30.)
    ; P.f= kailh_box (0., 1.5)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (75., -33.5, 15.)
    ; P.size= (17., 20., 6.) }

let k52 =
    { P.a= (pi /. 20., pi /. 20., -.pi /. 30.)
    ; P.f= kailh_box (0., 0.)
    ; P.cap= Some K.keycap_chery_r3
    ; P.p= (75.5, -7., 8.5)
    ; P.size= (17., 17., 6.) }

let k53 =
    { P.a= (pi /. 8., pi /. 20., -.pi /. 30.)
    ; P.f= kailh_box (-1.5, -1.5)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (78., 14., 13.)
    ; P.size= (20., 20., 6.) }

let k61 =
    { P.a= (-.pi /. 10., pi /. 20., -.pi /. 20.)
    ; P.f= kailh_box (-1.5, 1.5)
    ; P.cap= Some K.keycap_chery_r3
    ; P.p= (93., -32., 12.)
    ; P.size= (20., 20., 6.) }

let k62 =
    { P.a= (pi /. 20., 0., -.pi /. 15.)
    ; P.f= Model.Key_unit.dummy
    ; P.cap= None
    ; P.p= (96., -7., 6.)
    ; P.size= (19., 3., 4.) }

let k63 =
    { P.a= (pi /. 8., 0., -.pi /. 10.)
    ; P.f= kailh_box (-1.5, -1.5)
    ; P.cap= Some K.keycap_chery_r2
    ; P.p= (98.5, 2., 9.)
    ; P.size= (20., 20., 6.) }

open Scad_ml.Util

let under_cover d t f k =
    let open Model.Pad in
    { P.a= k.a
    ; P.f
    ; P.cap= None
    ; P.size= (get_x k.size, get_y k.size, t)
    ; P.p= k.p <+> Model.Matrix.trans (Model.Matrix.rot k.a) (0., 0., -.d -. t)
    }

let gen_cover t conf =
    conf
    |> List.map
         ~f:
           (List.map
              ~f:(Option.map ~f:(under_cover 3.5 t Model.Key_unit.dummy)))

let sub = M.union [M.hull [P.rside k43; P.barfl k53]]

module Patch = Model.Patch

let screw_k13 =
    { Patch.a= (pi /. 5., pi /. 15., pi /. 60.)
    ; Patch.p= (13.0, 33., 24.0)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 6.5
    ; Patch.insert_r= 1.5 }

let screw_k12 =
    { Patch.a= (pi /. 12., pi /. 20., pi /. 60.)
    ; Patch.p= (12.5, 0., 19.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_k20 =
    { Patch.a= (0., pi /. 20., pi /. 60.)
    ; Patch.p= (16., -25., 24.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_k33 =
    { Patch.a= (pi /. 12., pi /. 20., -.pi /. 60.)
    ; Patch.p= (33.0, 28., 17.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_k42 =
    { Patch.a= (pi /. 6., pi /. 20., -.pi /. 30.)
    ; Patch.p= (56.5, 26., 15.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_k53 =
    { Patch.a= (pi /. 6., pi /. 20., -.pi /. 30.)
    ; Patch.p= (100., 25., 16.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_k52 =
    { Patch.a= (pi /. 12., pi /. 20., -.pi /. 30.)
    ; Patch.p= (110., -7., 6.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 6.0
    ; Patch.insert_r= 1.5 }

let screw_k50 =
    { Patch.a= (0., pi /. 20., -.pi /. 10.)
    ; Patch.p= (94., -29.0, 10.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 6.5
    ; Patch.insert_r= 1.5 }

let screw_k30 =
    { Patch.a= (0., pi /. 20., 0.)
    ; Patch.p= (46., -20., 16.0)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_k40 =
    { Patch.a= (0., pi /. 20., -.pi /. 30.)
    ; Patch.p= (64., -26.5, 16.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_kp_l =
    { Patch.a= (0., pi /. 20., 0.)
    ; Patch.p= (47., -63., 28.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 4.5
    ; Patch.insert_r= 1.5 }

let screw_kp_r =
    { Patch.a= (0., pi /. 20., 0.)
    ; Patch.p= (67., -63., 25.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 4.5
    ; Patch.insert_r= 1.5 }

let screw_kt02f =
    { Patch.a= (0., 0., 0.)
    ; Patch.p= (-15., -68.7, 6.0)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 6.5
    ; Patch.insert_r= 1.5 }

let screw_kt02n =
    { Patch.a= (0., 0., 0.)
    ; Patch.p= (-30., -54., 6.0)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 6.5
    ; Patch.insert_r= 1.5 }

let screw_kt12f =
    { Patch.a= (0., 0., pi /. 6.)
    ; Patch.p= (-5., -37.0, 6.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 5.5
    ; Patch.insert_r= 1.5 }

let screw_kt12n =
    { Patch.a= (0., 0., pi /. 6.)
    ; Patch.p= (10., -50.0, 6.5)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 8.5
    ; Patch.insert_r= 1.5 }

let screw_kt22 =
    { Patch.a= (0., -3. *. pi /. 7., pi /. 6.)
    ; Patch.p= (22.0, -33., 36.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 8.5
    ; Patch.insert_r= 1.5 }

let screw_kt21 =
    { Patch.a= (0., -3. *. pi /. 7., 0.)
    ; Patch.p= (29., -54., 32.)
    ; Patch.out_r= 2.5
    ; Patch.in_r= 1.1
    ; Patch.top_h= 8.5
    ; Patch.insert_r= 1.5 }

let screw_top_bottom =
    [ Patch.Screw screw_k13
    ; Patch.Screw screw_k12
    ; Patch.Screw screw_k33
    ; Patch.Screw screw_k42
    ; Patch.Screw screw_k52
    ; Patch.Screw screw_k53
    ; Patch.Screw screw_k30
    ; Patch.Screw screw_k40
    ; Patch.Screw screw_kp_l
    ; Patch.Screw screw_kp_r
    ; Patch.Screw screw_k50
    ; Patch.Screw screw_k20 ]

let screw_thumb_top = []

let screw_thumb_bottom =
    [ Patch.Screw screw_kt22
    ; Patch.Screw screw_kt21
    ; Patch.Screw screw_kt12f
    ; Patch.Screw screw_kt12n
    ; Patch.Screw screw_kt02f
    ; Patch.Screw screw_kt02n ]

let kt12 =
    { P.a= (0., 0., pi /. 6.)
    ; P.f= kailh_choc (1., 0.)
    ; P.cap= Some K.keycap_choc
    ; P.p= (-9., -62., 7.)
    ; P.size= (18., 20., 6.) }

let kt02 =
    { P.a= (0., 0., pi /. 4.)
    ; P.f= kailh_choc (1., 0.)
    ; P.cap= Some K.keycap_choc
    ; P.p= (-23., -76., 7.)
    ; P.size= (18., 20., 6.) }

let kt21 =
    { P.a= (0., -3. *. pi /. 7., 0.)
    ; P.f= Model.Key_unit.dummy
    ; P.cap= None
    ; P.p= (23., -54., 18.)
    ; P.size= (20., 2., 6.) }

let kt22 =
    { P.a= (0., -3. *. pi /. 7., pi /. 6.)
    ; P.f= kailh_choc (-1.5, -1.)
    ; P.cap= Some K.keycap_choc
    ; P.p= (22., -47., 18.)
    ; P.size= (20., 19., 6.) }

let tmat =
    [ [None; None; Some kt02; None]
    ; [None; None; Some kt12; None]
    ; [None; Some kt21; Some kt22; None]
    ; [None; None; None; None] ]

let mat =
    [ [None; None; None; None; None; None; None]
    ; [None; None; Some k11; Some k12; Some k13; None; None]
    ; [None; None; Some k21; Some k22; Some k23; None; None]
    ; [None; None; Some k31; Some k32; Some k33; None; None]
    ; [None; Some kp; Some k41; Some k42; Some k43; None; None]
    ; [None; None; Some k51; Some k52; Some k53; None; None]
    ; [None; None; Some k61; Some k62; Some k63; None; None]
    ; [None; None; None; None; None; None; None] ]

let thumb_trackball tmat =
    Patch.apply_patches
      (M.union [P.ortho tmat])
      ( (screw_thumb_top |> List.map ~f:(fun p -> (p, Patch.Top)))
      @ (screw_thumb_bottom |> List.map ~f:(fun p -> (p, Patch.Top))) )

let dummy_key =
    { P.a= (0., 0., 0.)
    ; P.f= Model.Key_unit.dummy
    ; P.cap= None
    ; P.p= (0., 0., 0.)
    ; P.size= (0., 0., 0.) }

let thumb_cover = gen_cover 3.5 tmat

let idx pad c r =
    List.nth_exn (List.nth_exn pad c) r |> Option.value ~default:dummy_key

let tcover11 = idx thumb_cover 1 1

let tcover12 = idx thumb_cover 1 2

let tcover21 = idx thumb_cover 2 1

let tcover22 = idx thumb_cover 2 2

let top mat =
    Patch.apply_patches
      (M.union
         [ P.ortho mat
         ; sub
         ; M.hull [P.barfl kp; P.barnl k41; P.barnr k31]
         ; M.hull [P.barfl kp; P.nside k31] ])
      ( (screw_top_bottom |> List.map ~f:(fun p -> (p, Patch.Top)))
      @ (screw_thumb_top |> List.map ~f:(fun p -> (p, Patch.Bottom))) )

let palm_rest =
    let w = 75. in
    let d = 60. in
    let h = 18. in
    let hex_r = 5.0 in
    let hex_d = 2.0 in
    let hex =
        M.cylinder ~center:true ~fn:6 hex_r (h +. 0.02)
        |>> (0., 0., h /. 2.)
        |@> (0., 0., pi /. 2.)
    in
    let gen_hannycum n m =
        M.union
        @@ List.init m ~f:(fun i ->
               let col n =
                   M.union
                   @@ List.init n ~f:(fun i ->
                          hex
                          |>> ( ((hex_r *. 2. *. Float.cos (pi /. 6.)) +. hex_d)
                                *. float_of_int i
                              , 0.
                              , 0. ))
               in
               (if i mod 2 = 0 then col n else col (n - 1) |>> (hex_r, 0., 0.))
               |>> ( 0.
                   , (hex_d +. (2. *. hex_r *. (Float.cos (pi /. 6.) ** 2.)))
                     *. float_of_int i
                   , 0. ))
    in
    let k_w = 22. in
    let k_d = 20. in
    let k_h = 20. in
    let space = 5. in
    let lshift = 10. in
    M.union
      [ M.hull
          [ M.cube (k_w, k_d, k_h)
          ; M.cube (w, 0.01, h) |>> (-.lshift, -.space, 0.) ]
      ; M.difference
          (M.cube (w, d -. space, h))
          [gen_hannycum 6 5 |>> (10., 10., 0.)]
        |>> (-.lshift, -.d, 0.) ]

let bottom tmat mat =
    let tcover = gen_cover 3.5 tmat in
    let ortho = gen_cover 0.5 mat in
    let tidx = idx tcover in
    let kp_bottom = idx ortho 4 1 in
    let base =
        Patch.apply_patches
          (M.union
             [ P.ortho ortho
             ; P.proj @@ gen_cover 0.5 mat
             ; P.ortho @@ tcover
             ; M.hull
                 [ P.barfl kp_bottom
                 ; P.pbarfl kp_bottom
                 ; P.barnl @@ idx ortho 4 2
                 ; P.pbarnl @@ idx ortho 4 2
                 ; P.barnr @@ idx ortho 3 2
                 ; P.pbarnr @@ idx ortho 3 2 ]
             ; M.hull
                 [ P.barfl kp_bottom
                 ; P.pbarfl kp_bottom
                 ; P.nside @@ idx ortho 3 2
                 ; P.pnside @@ idx ortho 3 2 ]
             ; M.hull
                 [P.brside @@ tidx 1 1; P.blside @@ tidx 2 1; P.lside kp_bottom]
             ; M.hull [P.bottom @@ tidx 2 1; P.lside kp_bottom]
             ; M.hull
                 [P.bfside @@ tidx 2 1; P.bnside @@ tidx 2 2; P.barfl kp_bottom]
             ; M.hull
                 [ P.plside kp_bottom
                 ; P.blside kp_bottom
                 ; P.bnside @@ tidx 2 1
                 ; P.bfside @@ tidx 2 1
                 ; P.bnside @@ tidx 2 2 ]
             ; M.hull
                 [ P.plside kp_bottom
                 ; P.blside kp_bottom
                 ; P.bnside @@ tidx 2 2
                 ; P.blside @@ tidx 2 2 ]
             ; M.hull [P.plside @@ tidx 2 2; P.brside @@ tidx 1 2]
             ; M.hull [P.plside @@ tidx 2 2; P.plside @@ kp_bottom]
             ; M.hull [P.bottom @@ tidx 2 2; P.barfl kp_bottom]
             ; palm_rest |>> (45., -60., 0.) ])
          ( (screw_top_bottom |> List.map ~f:(fun p -> (p, Patch.Bottom)))
          @ (screw_thumb_bottom |> List.map ~f:(fun p -> (p, Patch.Bottom))) )
    in
    M.difference
      (M.union [base; Pcbmod.top |>> (-27.5, -25.5, 0.)])
      [ Pcbmod.hollow |>> (-27.5, -25.5, 0.)
      ; M.cube (19. *. 2., 24. *. 2., 5.0) |>> (20., -15., -0.01)
      ; M.cube (19., 23., 50.) |>> (kp_p <*> (1., 1., 0.) <+> (2., 5., 4.)) ]

let remove_cap =
    List.map
      ~f:
        (List.map ~f:(function
          | Some k -> Some {k with P.cap= None}
          | None -> None))

let () =
    build (bottom (remove_cap tmat) (remove_cap mat)) "bottom.scad" ;
    build (top (remove_cap mat)) "top.scad" ;
    build (thumb_trackball (remove_cap tmat)) "thumb.scad" ;
    build
      (M.union [top mat; bottom tmat mat; thumb_trackball tmat])
      "assembly.scad"
