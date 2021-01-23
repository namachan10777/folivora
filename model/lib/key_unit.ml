module M = Scad_ml.Model
open Scad_ml.Util

let kailh_box (dx, dy) (w, d, h) =
    let lump = M.cube (w, d, h) in
    M.difference (
        M.union [
            M.difference lump [
                M.cube (16., 16., 1.001)
                |>> ((w -. 16.)/.2. +. dx, (d -. 16.)/.2. +. dy, h -. 1.);
                M.cube (6., 16., h -. 1.0 -. 1.5 +. 0.001)
                |>> ((w -. 6.)/.2. +. dx, (d -. 16.)/.2. +. dy, -0.001);
            ];
            M.cube (16., 10., 1.)
            |>> ((w -. 16.)/.2. +. dx, (d -. 10.)/.2. +. dy, h -. 1.);
        ])
    [
        M.cube (14., 14., h +. 0.02)
        |>> ((w -. 14.)/.2. +. dx, (d -. 14.)/.2. +. dy, -0.01);
    ]

let kailh_choc (dx, dy) (w, d, h) =
    let lump = M.cube (w, d, h) in
    let cut = M.difference lump [
        M.cube (14., 14., h +. 0.02)
        |>> ((w -. 14.)/.2. +. dx, (d -. 14.)/.2. +. dy, -0.01);
        M.cube (16., 16., 1.01)
        |>> ((w -. 16.)/.2. +. dx, (d -. 16.)/.2. +. dy, h -. 1.);
        M.cube (16., 14., h -. 1.0 -. 1.3 +. 0.001)
        |>> ((w -. 16.)/.2. +. dx, (d -. 16.)/.2. +. dy, -0.001);
    ]
    in M.union [
        cut;
        M.difference (
            M.union [
                M.cube (16., 10., 1.)
                |>> ((w -. 16.)/.2. +. dx, (d -. 10.)/.2. +. dy, h -. 1.);
                M.cube (16., 3., h)
                |>> ((w -. 16.)/.2. +. dx, (d -. 3.)/.2. +. dy, 0.);
            ]
        ) [
            M.cube (14., 14., h +. 0.02)
            |>> ((w -. 14.)/.2. +. dx, (d -. 14.)/.2. +. dy, -0.01);
        ];
    ]

let bittradeone_trackball (dx, dy) (w, d, h) =
    let lump = M.cube (w, d, h +. 6.) in
    M.difference lump [
        M.cube (17., 17., 6. +. 0.001)
        |>> ((w -. 17.)/.2. +. dx, (d -. 17.)/.2. +. dy, h);
        M.cube (6.35, 1.27, h +. 6. +. 0.02)
        |>> ((w -. 17.)/.2. +. dx +. 7.5, (d -. 17.)/.2. +. dy +. 1.0, -0.01);
    ]

let dummy = M.cube
