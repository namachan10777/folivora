let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let kailh_box = Model.Key_unit.kailh_box
let kailh_choc = Model.Key_unit.kailh_choc
let pi = Scad_ml.Core.pi

module P = Model.Pad
module M = Scad_ml.Model
open Core
open Scad_ml.Util

let cylinder_top = M.cylinder 3.0 3.0 ~center:true
let cylinder_bottom = M.cylinder 3.0 3.0 ~center:true
    |>> (0., 0., -.3.)

let k11 = {
    P.a = (-.pi/.5., pi/.20. +. pi/.20., pi/.30.);
    P.f = kailh_box (1.5, 1.5);
    P.p = (-2., -23., 37.5);
    P.size = (20., 20., 6.);
}

let k12 = {
    P.a = (0., pi/.12. +. pi/.20., pi/.30.);
    P.f = kailh_box (1.5, 0.);
    P.p = (-10., -1., 25.5);
    P.size = (20., 17., 6.);
}

let k13 = {
    P.a = (pi/.12., pi/.12. +. pi/.20., pi/.30.);
    P.f = kailh_box (1.5, 0.);
    P.p = (-12., 18., 25.5);
    P.size = (20., 17., 6.);
}

let k14 = {
    P.a = (pi/.6., pi/.12. +. pi/.20., pi/.30.);
    P.f = kailh_box (1.5, -1.5);
    P.p = (-12., 37., 30.5);
    P.size = (20., 20., 6.);
}

let k20 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = kailh_choc (0., 0.);
    P.p = (18., -39., 30.);
    P.size = (17., 17., 6.);
}


let k21 = {
    P.a = (-.pi/.5., pi/.20., pi/.60.);
    P.f = kailh_box (0., 1.5);
    P.p = (18., -22., 31.5);
    P.size = (17., 20., 6.);
}

let k22 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = kailh_box (0., 0.);
    P.p = (14., 1., 16.);
    P.size = (17., 17., 6.);
}

let k23 = {
    P.a = (pi/.12., pi/.20., pi/.60.);
    P.f = kailh_box (0., 0.);
    P.p = (12.5, 20., 16.);
    P.size = (17., 17., 6.);
}

let k24 = {
    P.a = (pi/.6., pi/.20., pi/.60.);
    P.f = kailh_box (0., -1.5);
    P.p = (12., 39., 21.);
    P.size = (17., 20., 6.);
}

let kp = {
    P.a = (0., pi/.20., 0.);
    P.f = kailh_choc (0., 0.);
    P.p = (45., -67., 30.0);
    P.size = (24., 24., 6.);
}

let k30 = {
    P.a = (0., pi/.20., 0.);
    P.f = kailh_choc (0., 0.);
    P.p = (36., -37., 25.0);
    P.size = (17., 17., 6.);
}

let k31 = {
    P.a = (-.pi/.5., pi/.20., 0.);
    P.f = kailh_box (0., 0.);
    P.p = (36., -16., 24.);
    P.size = (17., 17., 6.);
}

let k32 = {
    P.a = (0., pi/.20., 0.);
    P.f = kailh_box (0., 0.);
    P.p = (31., 6., 10.);
    P.size = (17., 17., 6.);
}

let k33 = {
    P.a = (pi/.12., pi/.20., 0.);
    P.f = kailh_box (0., 0.);
    P.p = (31., 25., 10.);
    P.size = (17., 17., 6.);
}

let k34 = {
    P.a = (pi/.6., pi/.20., 0.);
    P.f = kailh_box (0., -1.5);
    P.p = (31.5, 44., 15.);
    P.size = (17., 20., 6.);
}

let k40 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = kailh_choc (0., 0.);
    P.p = (54., -42., 23.5);
    P.size = (17., 17., 6.);
}

let k41 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.60.);
    P.f = kailh_box (0., 0.);
    P.p = (54., -20., 21.);
    P.size = (17., 17., 6.);
}


let k42 = {
    P.a = (0., pi/.20., -.pi/.60.);
    P.f = kailh_box (0., 0.);
    P.p = (50., 2., 8.);
    P.size = (17., 17., 6.);
}

let k43 = {
    P.a = (pi/.12., pi/.20., -.pi/.60.);
    P.f = kailh_box (0., 0.);
    P.p = (51., 21., 8.);
    P.size = (17., 17., 6.);
}

let k44 = {
    P.a = (pi/.6., pi/.20., -.pi/.60.);
    P.f = kailh_box (-1.5, -1.5);
    P.p = (52., 40., 13.);
    P.size = (20., 20., 6.);
}

let k50 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = kailh_choc (-1., 0.);
    P.p = (73., -39., 20.5);
    P.size = (19., 17., 6.);
}

let k51 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.30.);
    P.f = kailh_box (0., 1.5);
    P.p = (73., -26., 23.);
    P.size = (17., 20., 6.);
}

let k52 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = kailh_box (0., 0.);
    P.p = (71., -3., 10.);
    P.size = (17., 17., 6.);
}

let k53 = {
    P.a = (pi/.12., pi/.20., -.pi/.30.);
    P.f = kailh_box (-1.5, -1.5);
    P.p = (73., 16., 10.);
    P.size = (20., 20., 6.);
}

let k61 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.10.);
    P.f = kailh_box (-1.5, 1.5);
    P.p = (90., -29., 21.);
    P.size = (20., 20., 6.);
}

let k62 = {
    P.a = (0., 0., -.pi/.10.);
    P.f = kailh_box (-1.5, 0.);
    P.p = (95., -6., 7.);
    P.size = (20., 17., 6.);
}

let k63 = {
    P.a = (pi/.12., 0., -.pi/.10.);
    P.f = kailh_box (-1.5, -1.5);
    P.p = (101., 13., 7.);
    P.size = (20., 20., 6.);
}

let k64 = {
    P.a = (pi/.6., 0., -.pi/.10.);
    P.f = kailh_box (-3., 0.);
    P.p = (105., 32., 12.);
    P.size = (20., 17., 6.);
}

open Scad_ml.Util

let under_cover d t f k =
    let open Model.Pad in
    {
        P.a = k.a;
        P.f = f;
        P.size = (get_x k.size, get_y k.size, t);
        P.p = (k.p <+> (Model.Matrix.trans (Model.Matrix.rot k.a) (0., 0., -.d -. t)));
    }


let gen_cover t conf =
    conf
    |> List.map ~f:(List.map ~f:(Option.map ~f:(under_cover 3.5 t Model.Key_unit.dummy)))

let sub = M.union [
    M.hull [
        P.rside k43;
        P.barfl k53;
    ];
]

module Patch = Model.Patch
let screw_k13 = {
    Patch.a = (0., pi/.15., pi/.60.);
    Patch.p = (10., 34., 18.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw_k12 = {
    Patch.a = (0., pi/.20., pi/.60.);
    Patch.p = (11.2, 2., 16.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw_k33 = {
    Patch.a = (pi/.12., pi/.20., -.pi/.60.);
    Patch.p = (50., 32., 8.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =9.5;
}

let screw_k42 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (69.5, 10., 5.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}


let screw_k52 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (92., -3., 6.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}


let screw_k53 = {
    Patch.a = (pi/.12., pi/.20., -.pi/.30.);
    Patch.p = (97., 24., 9.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw_thumb_bridge2 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (30., -62., 20.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw_thumb_bridge1 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (20., -47., 24.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =9.5;
}

let screw_k30 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (46., -17., 23.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =5.5;
}


let screw_k40 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (64., -22., 18.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw_kp_l = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (45., -62., 27.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =9.;
}

let screw_kp_r = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (69., -62., 23.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =9.;
}


let screw_k50 = {
    Patch.a = (0., pi/.20., -.pi/.10.);
    Patch.p = (92., -28.5, 17.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw_kt11 = {
    Patch.a = (0., -.1.*.pi/.2., 0.);
    Patch.p = (13.5, -76., 4.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =14.5;
}

let screw_kt12 = {
    Patch.a = (0., -.1.*.pi/.2., pi/.6.);
    Patch.p = (5., -32., 3.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw_k20 = {
    Patch.a = (0., pi/.20., pi/.60.);
    Patch.p = (25., -20., 26.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw_kt22 = {
    Patch.a = (0., -.3.*.pi/.7., pi/.6.);
    Patch.p = (10.5, -33., 45.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw_kt21 = {
    Patch.a = (0., -.3.*.pi/.7., 0.);
    Patch.p = (16., -74., 32.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw_top_bottom = [
    Patch.Screw screw_k13;
    Patch.Screw screw_k12;
    Patch.Screw screw_k33;
    Patch.Screw screw_k42;
    Patch.Screw screw_k52;
    Patch.Screw screw_k53;
    Patch.Screw screw_k30;
    Patch.Screw screw_k40;
    Patch.Screw screw_kp_l;
    Patch.Screw screw_kp_r;
    Patch.Screw screw_k50;
    Patch.Screw screw_k20;
]

let screw_thumb_top = [

]

let screw_thumb_bottom = [
    Patch.Screw screw_kt22;
    Patch.Screw screw_kt21;
    Patch.Screw screw_kt12;
    Patch.Screw screw_kt11;
]

let kt11 = {
    P.a = (0., -.1.*.pi/.2., 0.);
    P.f = Model.Key_unit.bittradeone_trackball (0., 0.);
    P.p = (11., -76., 0.);
    P.size = (19., 22., 6.);
}

let kt12 = {
    P.a = (0., -.1.*.pi/.2.,  pi/.6.);
    P.f = kailh_choc (3., 0.);
    P.p = (10., -47., 0.);
    P.size = (23., 17., 6.);
}

let kt21 = {
    P.a = (0., -.3.*.pi/.7., 0.);
    P.f = kailh_choc (-1.5, 1.5);
    P.p = (11., -74., 22.);
    P.size = (20., 20., 6.);
}

let kt22 = {
    P.a = (0., -.3.*.pi/.7., pi/.6.);
    P.f = kailh_choc (-1.5, 0.);
    P.p = (10., -47., 26.);
    P.size = (20., 17., 6.);
}

let tmat = [
    [None; None;       None;       None];
    [None; Some(kt11); Some(kt12); None];
    [None; Some(kt21); Some(kt22); None];
    [None; None;       None;       None];
]

let mat = [
    [None;    None;     None;     None;     None;     None;     None];
    [None;    None;     Some(k11);Some(k12);Some(k13);None;     None];
    [None;    None;     Some(k21);Some(k22);Some(k23);None;     None];
    [None;    None;     Some(k31);Some(k32);Some(k33);None;     None];
    [None;    Some(kp); Some(k41);Some(k42);Some(k43);None;     None];
    [None;    None;     Some(k51);Some(k52);Some(k53);None;     None];
    [None;    None;     Some(k61);Some(k62);Some(k63);None;     None];
    [None;    None;     None;     None;     None;     None;     None];
]

let thumb = 
    Patch.apply_patches
        (M.union [P.ortho tmat])
        ((screw_thumb_top |> List.map ~f:(fun p -> (p, Patch.Top)))
        @ (screw_thumb_bottom |> List.map ~f:(fun p -> (p, Patch.Top))))

let dummy_key = {
    P.a = (0., 0., 0.);
    P.f = Model.Key_unit.dummy;
    P.p = (0., 0., 0.);
    P.size = (0., 0., 0.);
}

let thumb_cover = gen_cover 3.5 tmat
let idx pad c r = List.nth_exn (List.nth_exn pad c) r |> Option.value ~default:dummy_key
let tcover11 = idx thumb_cover 1 1
let tcover12 = idx thumb_cover 1 2
let tcover21 = idx thumb_cover 2 1
let tcover22 = idx thumb_cover 2 2

let top =
    Patch.apply_patches
        (M.union [
            P.ortho mat;
            sub;
            M.hull [
                P.barfl kp;
                P.barnl k41;
                P.barnr k31;
            ];
            M.hull [
                P.barfl kp;
                P.nside k31;
            ];
        ])
        ((screw_top_bottom |> List.map ~f:(fun p -> (p, Patch.Top)))
        @ (screw_thumb_top |> List.map ~f:(fun p -> (p, Patch.Bottom))))



let bottom =
    let tcover = gen_cover 3.5 tmat in
    let ortho = gen_cover 0.5 mat in
    let tidx = idx tcover in
    let kp_bottom = idx ortho 4 1 in
    let base = Patch.apply_patches
        (M.union [
            P.ortho ortho;
            P.proj @@ gen_cover 0.5 mat;
            P.ortho @@ tcover;
            M.hull [
                P.barfl kp_bottom;
                P.pbarfl kp_bottom;
                P.barnl @@ idx ortho 4 2;
                P.pbarnl @@ idx ortho 4 2;
                P.barnr @@ idx ortho 3 2;
                P.pbarnr @@ idx ortho 3 2;
            ];
            M.hull [
                P.barfl kp_bottom;
                P.pbarfl kp_bottom;
                P.nside @@ idx ortho 3 2;
                P.pnside @@ idx ortho 3 2;
            ];
            M.hull [
                P.bottom @@ tidx 1 1;
                P.lside kp_bottom;
                P.plside kp_bottom;
            ];
            M.hull [
                P.brside @@ tidx 1 1;
                P.blside @@ tidx 2 1;
                P.lside kp_bottom;
            ];
            M.hull [
                P.bottom @@ tidx 2 1;
                P.lside kp_bottom;
            ];
            M.hull [
                P.bfside @@ tidx 2 1;
                P.bnside @@ tidx 2 2;
                P.barfl kp_bottom;
            ];
            M.hull [
                P.bfside @@ tidx 1 1;
                P.bnside @@ tidx 1 2;
                P.barfl kp_bottom;
                P.pbarfl kp_bottom;
            ];
            M.hull [
                P.bottom @@ tidx 1 2;
                P.barfl kp_bottom;
                P.pbarfl kp_bottom;
            ];
            M.hull [
                P.bottom @@ tidx 2 2;
                P.barfl kp_bottom;
            ];
            M.hull [
                P.blside @@ tidx 2 2;
                P.brside @@ tidx 1 2;
                P.barfl kp_bottom;
            ];
            M.hull [
                P.bbarfr @@ tidx 1 1;
                P.bbarfl @@ tidx 2 1;
                P.bbarnl @@ tidx 2 2;
                P.bbarnr @@ tidx 1 2;
                P.barfl kp_bottom;
            ];
        ])
        ((screw_top_bottom |> List.map ~f:(fun p -> (p, Patch.Bottom)))
        @ (screw_thumb_bottom |> List.map ~f:(fun p -> (p, Patch.Bottom))))
    in
        M.difference
            (M.union [
                base;
                Pcbmod.top |>> (-27.5, -25.5, 0.);
            ]) [
            Pcbmod.hollow |>> (-27.5, -25.5, 0.);
            ]

let () =
    build (M.union [bottom]) "bottom.scad";
    build (M.union [top]) "top.scad";
    build (M.union [thumb]) "thumb.scad";
    build (M.union [top; bottom; thumb]) "assembly.scad";
