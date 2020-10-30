module M = Scad_ml.Model
open Scad_ml.Util

type key_conf_t = {
    p : Scad_ml.Math.t;
    a : Scad_ml.Math.t;
    size : Scad_ml.Math.t;
    f : Scad_ml.Math.t -> Scad_ml.Core.scad_t;
}

let barfr k =
    let (w, d, h) = k.size in
    M.cube (0.001, 0.001, h) |>> (w, d, 0.) |@> k.a |>> k.p

let barfl k =
    let (_, d, h) = k.size in
    M.cube (0.001, 0.001, h) |>> (0., d, 0.) |@> k.a |>> k.p

let barnr k =
    let (w, _, h) = k.size in
    M.cube (0.001, 0.001, h) |>> (w, 0., 0.) |@> k.a |>> k.p

let barnl k =
    let (_, _, h) = k.size in
    M.cube (0.001, 0.001, h) |@> k.a |>> k.p

let bfside k =
    let (w, d, _) = k.size in
    M.cube (w, 0.001, 0.001) |>> (0., d, 0.) |@> k.a |>> k.p

let bnside k =
    let (w, _, _) = k.size in
    M.cube (w, 0.001, 0.001) |@> k.a |>> k.p

let blside k =
    let (_, d, _) = k.size in
    M.cube (0.001, d, 0.001) |@> k.a |>> k.p

let brside k =
    let (w, d, _) = k.size in
    M.cube (0.001, d, 0.001) |>> (w, 0., 0.) |@> k.a |>> k.p

let fside k =
    let (w, d, h) = k.size in
    M.cube (w, 0.001, h) |>> (0., d, 0.) |@> k.a |>> k.p

let nside k =
    let (w, _, h) = k.size in
    M.cube (w, 0.001, h) |@> k.a |>> k.p

let lside k =
    let (_, d, h) = k.size in
    M.cube (0.001, d, h) |@> k.a |>> k.p

let rside k =
    let (w, d, h) = k.size in
    M.cube (0.001, d, h) |>> (w, 0., 0.) |@> k.a |>> k.p

let bottom k =
    let (w, d, _) = k.size in
    M.cube (w, d, 0.001) |@> k.a |>> k.p

let body k =
    k.f k.size |@> k.a |>> k.p

let lhalf k =
    let (w, d, h) = k.size in
    {
        k with
        size = (w/.2., d, h);
    }

let rec vjoint_col = function
    | [] -> []
    | _ :: [] -> []
    | None :: mat -> vjoint_col mat
    | _ :: None :: mat -> vjoint_col mat
    | Some(k1) :: Some(k2) :: mat ->
        (M.hull [fside k1; nside k2])
        :: vjoint_col (Some(k2) :: mat)

let rec hjoint_col c1 c2 = match (c1, c2) with
    | ([], []) -> []
    | (None :: c1, None :: c2) -> hjoint_col c1 c2

    | (None :: [], Some _ :: []) ->
        []
    | (Some _ :: [], None :: []) ->
        []

    | (Some(k1) :: [], Some(k2) :: []) ->
        [M.hull [rside k1; lside k2]]

    | (Some(k1) :: None :: c1, Some(k2) :: None :: c2) ->
        (M.hull [rside k1; lside k2])
        :: hjoint_col c1 c2

    | (Some(k1) :: Some(k1') :: mat1, Some(k2) :: Some(k2') :: mat2) ->
        (M.hull [rside k1; lside k2])
        :: (M.hull [barfr k1; barnr k1'; barfl k2; barnl k2'])
        :: hjoint_col (Some(k1')::mat1) (Some(k2')::mat2)

    | (Some(k1) :: Some(k1') :: c1, Some(k2) :: None :: c2) ->
        (M.hull [rside k1; lside k2])
        :: (M.hull [barfr k1; barnr k1'; barfl k2])
        :: hjoint_col c1 c2

    | (Some(k1) :: None :: c1, Some(k2) :: Some(k2') :: c2) ->
        (M.hull [rside k1; lside k2])
        :: (M.hull [barfr k1; barnr k2'; barfl k2])
        :: hjoint_col c1 c2

    | (None :: c1, Some _ :: c2) -> hjoint_col c1 c2
    | (Some _ :: c1, None :: c2) -> hjoint_col c1 c2
    | (_ :: _, []) -> []
    | ([], _ :: _) -> []
    | (_ :: _, _ :: _ :: _ :: []) -> []
    | (_ :: _ :: _ :: [],  _ :: _) -> []
    | _ -> []

let rec place_col = function
    | [] -> []
    | Some(k) :: col -> (body k) :: place_col col
    | None :: col -> place_col col

let ortho mat =
    let rec f = function
    | [] -> []
    | col :: [] -> (vjoint_col col) @ (place_col col)
    | col1 :: col2 :: mat -> (hjoint_col col1 col2) @(vjoint_col col1) @ (place_col col1) @ f (col2 :: mat)
    in f mat |> M.union
