open Core
open Scad_ml.Util
module M = Scad_ml.Model

type screw_conf_t = {
    out_r: float;
    in_r: float;
    top_h: float;
    bottom_h: float;
    p: Scad_ml.Math.t;
    a: Scad_ml.Math.t;
}

type patch_t =
    | Screw of screw_conf_t

type patch_target_t = Bottom | Top

type patch_conf_t = {
    target: patch_target_t;
}

let apply_patches conf target patches =
    let (union, diff) =
        patches
        |> List.map ~f:(fun patch -> match (patch, conf.target) with
            | (Screw screw, Top) ->
                let outer = M.cylinder ~center:true ~fn:30 screw.out_r screw.top_h
                    |>> (0., 0., screw.top_h /. 2.) |@> screw.a |>> screw.p
                in
                let top_cut = M.cylinder ~center:true ~fn:30 screw.out_r 3.0
                    |>> (0., 0., screw.top_h +. 1.5) |@> screw.a |>> screw.p
                in
                let inner = M.cylinder ~center:true ~fn:30 screw.in_r (screw.top_h +. 0.02)
                    |>> (0., 0., screw.top_h /. 2. +. 0.01) |@> screw.a |>> screw.p
                in
                (outer, M.union [inner; top_cut])
            | (Screw screw, Bottom) ->
                let outer = M.cylinder ~center:true ~fn:30 screw.out_r screw.bottom_h
                    |>> (0., 0., -.screw.bottom_h /. 2.) |@> screw.a |>> screw.p
                in
                let bottom_cut = M.cylinder ~center:true ~fn:30 screw.out_r 3.0
                    |>> (0., 0., -.screw.bottom_h -. 1.5) |@> screw.a |>> screw.p
                in
                let inner = M.cylinder ~center:true ~fn:30 screw.in_r (screw.bottom_h +. 0.02)
                    |>> (0., 0., -.screw.bottom_h /. 2. -. 0.01) |@> screw.a |>> screw.p
                in
                (outer, M.union [inner; bottom_cut])
        )
        |> List.unzip
    in M.difference (M.union (target :: union)) diff
