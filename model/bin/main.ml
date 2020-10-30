let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Model.Key_unit.cherry_mx
let pi = Scad_ml.Core.pi

module P = Model.Pad
module M = Scad_ml.Model
open Core
open Scad_ml.Util

let cylinder_top = M.cylinder 3.0 3.0 ~center:true
let cylinder_bottom = M.cylinder 3.0 3.0 ~center:true
    |>> (0., 0., -.3.)

let k100 = {
    P.a = (0., pi/.20. +. pi/.20., pi/.30.);
    P.f = Model.Key_unit.dummy;
    P.p = (1., -20., 47.5);
    P.size = (0., 0., 0.);
}

let k10 = {
    P.a = (-.pi/.5., pi/.20. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (1., -21., 35.5);
    P.size = (16., 16., 6.);
}

let k11 = {
    P.a = (0., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-7., -1., 24.5);
    P.size = (16., 16., 6.);
}

let k12 = {
    P.a = (pi/.12., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-9., 18., 24.5);
    P.size = (16., 16., 6.);
}

let k13 = {
    P.a = (pi/.6., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-9., 37., 29.5);
    P.size = (16., 16., 6.);
}

let k200 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = Model.Key_unit.dummy;
    P.p = (18., -20., 30.5);
    P.size = (16., 0., 3.);
}


let k20 = {
    P.a = (-.pi/.5., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (18., -20., 31.);
    P.size = (16., 16., 6.);
}

let k21 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (14., 1., 16.);
    P.size = (16., 16., 6.);
}

let k22 = {
    P.a = (pi/.12., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (12.5, 20., 16.);
    P.size = (16., 16., 6.);
}

let k23 = {
    P.a = (pi/.6., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (12., 39., 21.);
    P.size = (16., 16., 6.);
}

let kp = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (45., -67., 19.0);
    P.size = (16., 16., 6.);
}

let k300 = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (36., -37., 25.0);
    P.size = (16., 16., 6.);
}

let k30 = {
    P.a = (-.pi/.5., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (36., -16., 24.);
    P.size = (16., 16., 6.);
}

let k31 = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (31., 6., 10.);
    P.size = (16., 16., 6.);
}

let k32 = {
    P.a = (pi/.12., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (31., 25., 10.);
    P.size = (16., 16., 6.);
}

let k33 = {
    P.a = (pi/.6., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (31.5, 44., 15.);
    P.size = (16., 16., 6.);
}

let k400 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (54., -42., 23.5);
    P.size = (16., 16., 6.);
}

let k40 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (54., -20., 21.);
    P.size = (16., 16., 6.);
}


let k41 = {
    P.a = (0., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (50., 2., 8.);
    P.size = (16., 16., 6.);
}

let k42 = {
    P.a = (pi/.12., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (51., 21., 8.);
    P.size = (16., 16., 6.);
}

let k43 = {
    P.a = (pi/.6., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (52., 40., 13.);
    P.size = (16., 16., 6.);
}

let k500 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., -39., 20.5);
    P.size = (16., 16., 6.);
}

let k50 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., -23., 22.);
    P.size = (16., 16., 6.);
}

let k51 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (71., -3., 10.);
    P.size = (16., 16., 6.);
}

let k52 = {
    P.a = (pi/.12., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., 16., 10.);
    P.size = (16., 16., 6.);
}

let k53 = {
    P.a = (pi/.6., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (77., 35., 15.);
    P.size = (16., 16., 6.);
}

let k600 = {
    P.a = (-.pi/.2., pi/.20., -.pi/.10.);
    P.f = Model.Key_unit.dummy;
    P.p = (94., -16., 32.);
    P.size = (0., 0., 0.);
}
let k60 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (90., -26., 20.);
    P.size = (16., 16., 6.);
}

let k61 = {
    P.a = (0., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (95., -6., 7.);
    P.size = (16., 16., 6.);
}

let k62 = {
    P.a = (pi/.12., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (101., 13., 7.);
    P.size = (16., 16., 6.);
}

let k63 = {
    P.a = (pi/.6., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (105., 32., 12.);
    P.size = (16., 16., 6.);
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

let mat = [
    [None;     None;      Some(k10); Some(k11); Some(k12); Some(k13)];
    [None;     None;      Some(k20); Some(k21); Some(k22); Some(k23)];
    [None;     Some(k300);Some(k30); Some(k31); Some(k32); Some(k33)];
    [Some(kp); Some(k400);Some(k40); Some(k41); Some(k42); Some(k43)];
    [None;     Some(k500);Some(k50); Some(k51); Some(k52); None];
    [None;     None;      Some(k60); Some(k61); Some(k62); None];
]

let mat_covered = [
    [k11; k12; k13];
    [k21; k22; k23];
    [k31; k32; k33];
    [k41; k42; k43];
    [k51; k52];
    [k61; k62];
]

let gen_cover conf =
    conf
    |> List.map ~f:(List.map ~f:(Option.map ~f:(under_cover 1.5 2.5 Model.Key_unit.dummy)))

let sub = M.union [
    M.hull [
        P.rside k43;
        P.barfl k52;
    ];
]

module Patch = Model.Patch
let screw1 = {
    Patch.a = (pi/.6., pi/.15., pi/.60.);
    Patch.p = (9.2, 39., 21.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
    Patch.bottom_h = 4.5;
}

let screw2 = {
    Patch.a = (0., pi/.20., pi/.60.);
    Patch.p = (11.2, 4., 16.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
    Patch.bottom_h = 4.;
}

let screw3 = {
    Patch.a = (pi/.12., pi/.20., -.pi/.60.);
    Patch.p = (50., 32., 13.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
    Patch.bottom_h = 7.5;
}

let screw4 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (69.5, 10., 9.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
    Patch.bottom_h = 6.;
}


let screw5 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (92., -3., 9.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
    Patch.bottom_h = 6.;
}


let screw6 = {
    Patch.a = (pi/.12., pi/.20., -.pi/.30.);
    Patch.p = (97., 24., 12.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
    Patch.bottom_h = 6.;
}

let screw_set = [
    Patch.Screw screw1; Patch.Screw screw2;
    Patch.Screw screw3;
    Patch.Screw screw4;
    Patch.Screw screw5; Patch.Screw screw6;
]

let kt11 = {
    P.a = (0., -.1.*.pi/.2., 0.);
    P.f = cherry_mx;
    P.p = (21., -61., 5.);
    P.size = (16., 16., 6.);
}

let kt12 = {
    P.a = (0., -.1.*.pi/.2.,  pi/.6.);
    P.f = cherry_mx;
    P.p = (20., -37., 5.);
    P.size = (16., 16., 6.);
}

let kt21 = {
    P.a = (0., -.4.*.pi/.7., 0.);
    P.f = cherry_mx;
    P.p = (21., -61., 25.);
    P.size = (16., 16., 6.);
}

let kt22 = {
    P.a = (0., -.4.*.pi/.7., pi/.6.);
    P.f = cherry_mx;
    P.p = (20., -37., 25.);
    P.size = (16., 16., 6.);
}

let tmat = [
    [Some(kt11); Some(kt12)];
    [Some(kt21); Some(kt22)];
]

let thumb = 
    Patch.apply_patches
        { Patch.target = Patch.Bottom; }
        (M.union [P.ortho tmat])
        []

let thumb_cover = gen_cover tmat
let idx pad c r = List.nth_exn (List.nth_exn pad c) r
let tcover11 = idx thumb_cover 0 0
let tcover12 = idx thumb_cover 0 1
let tcover21 = idx thumb_cover 1 0
let tcover22 = idx thumb_cover 1 1

let top =
    Patch.apply_patches
        { Patch.target = Patch.Top; }
        (M.union [
            P.ortho mat;
            P.ortho tmat;
            sub;
            P.ortho @@ gen_cover tmat;
            M.hull [
                P.bnside @@ P.lhalf @@ Option.value_exn tcover22;
                P.bfside @@ P.lhalf @@ Option.value_exn tcover21;
                P.barnl k300;
            ];
            M.hull [
                P.bottom @@ P.lhalf @@ Option.value_exn tcover21;
                P.barnl k300;
                P.barfl kp;
            ];
            M.hull [
                P.bnside @@ P.lhalf @@ Option.value_exn tcover21;
                P.lside kp;
            ];
            M.hull [
                P.barfl kp;
                P.nside k300;
            ];
            M.hull [
                P.barnr k300;
                P.barnl k400;
                P.fside kp;
            ];
        ])
        screw_set

let bottom =
    Patch.apply_patches
        { Patch.target = Patch.Bottom; }
        (M.union [P.ortho @@ gen_cover mat])
        screw_set

let () =
    build (M.union [bottom; top]) "pad.scad"
