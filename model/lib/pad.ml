module M = Scad_ml.Model
open Scad_ml.Util

type key_conf_t =
    { p: Scad_ml.Math.t
    ; a: Scad_ml.Math.t
    ; size: Scad_ml.Math.t
    ; f: Scad_ml.Math.t -> Scad_ml.Core.scad_t }

let bbarfr k =
    let w, d, _ = k.size in
    M.cube (0.001, 0.001, 0.001) |>> (w, d, 0.) |@> k.a |>> k.p

let bbarfl k =
    let _, d, _ = k.size in
    M.cube (0.001, 0.001, 0.001) |>> (0., d, 0.) |@> k.a |>> k.p

let bbarnr k =
    let w, _, _ = k.size in
    M.cube (0.001, 0.001, 0.001) |>> (w, 0., 0.) |@> k.a |>> k.p

let bbarnl k = M.cube (0.001, 0.001, 0.001) |@> k.a |>> k.p

let barfr k =
    let w, d, h = k.size in
    M.cube (0.001, 0.001, h) |>> (w, d, 0.) |@> k.a |>> k.p

let barfl k =
    let _, d, h = k.size in
    M.cube (0.001, 0.001, h) |>> (0., d, 0.) |@> k.a |>> k.p

let barnr k =
    let w, _, h = k.size in
    M.cube (0.001, 0.001, h) |>> (w, 0., 0.) |@> k.a |>> k.p

let barnl k =
    let _, _, h = k.size in
    M.cube (0.001, 0.001, h) |@> k.a |>> k.p

let bfside k =
    let w, d, _ = k.size in
    M.cube (w, 0.001, 0.001) |>> (0., d, 0.) |@> k.a |>> k.p

let bnside k =
    let w, _, _ = k.size in
    M.cube (w, 0.001, 0.001) |@> k.a |>> k.p

let blside k =
    let _, d, _ = k.size in
    M.cube (0.001, d, 0.001) |@> k.a |>> k.p

let brside k =
    let w, d, _ = k.size in
    M.cube (0.001, d, 0.001) |>> (w, 0., 0.) |@> k.a |>> k.p

let fside k =
    let w, d, h = k.size in
    M.cube (w, 0.001, h) |>> (0., d, 0.) |@> k.a |>> k.p

let nside k =
    let w, _, h = k.size in
    M.cube (w, 0.001, h) |@> k.a |>> k.p

let lside k =
    let _, d, h = k.size in
    M.cube (0.001, d, h) |@> k.a |>> k.p

let rside k =
    let w, d, h = k.size in
    M.cube (0.001, d, h) |>> (w, 0., 0.) |@> k.a |>> k.p

let bottom k =
    let w, d, _ = k.size in
    M.cube (w, d, 0.001) |@> k.a |>> k.p

let body k = k.f k.size |@> k.a |>> k.p

let lhalf k =
    let w, d, h = k.size in
    {k with size= (w /. 2., d, h)}

let rec vjoint_col = function
    | [] -> []
    | [_] -> []
    | None :: mat -> vjoint_col mat
    | _ :: None :: mat -> vjoint_col mat
    | Some k1 :: Some k2 :: mat ->
        M.hull [fside k1; nside k2] :: vjoint_col (Some k2 :: mat)

let rec hjoint_col c1 c2 =
    match (c1, c2) with
    | [], [] -> []
    | None :: c1, None :: c2 -> hjoint_col c1 c2
    | [None], [Some _] -> []
    | [Some _], [None] -> []
    | [Some k1], [Some k2] -> [M.hull [rside k1; lside k2]]
    | Some k1 :: None :: c1, Some k2 :: None :: c2 ->
        M.hull [rside k1; lside k2] :: hjoint_col c1 c2
    | Some k1 :: Some k1' :: mat1, Some k2 :: Some k2' :: mat2 ->
        M.hull [rside k1; lside k2]
        :: M.hull [barfr k1; barnr k1'; barfl k2; barnl k2']
        :: hjoint_col (Some k1' :: mat1) (Some k2' :: mat2)
    | Some k1 :: Some k1' :: c1, Some k2 :: None :: c2 ->
        M.hull [rside k1; lside k2]
        :: M.hull [barfr k1; barnr k1'; barfl k2]
        :: hjoint_col c1 c2
    | Some k1 :: None :: c1, Some k2 :: Some k2' :: c2 ->
        M.hull [rside k1; lside k2]
        :: M.hull [barfr k1; barnr k2'; barfl k2]
        :: hjoint_col c1 c2
    | None :: c1, Some _ :: c2 -> hjoint_col c1 c2
    | Some _ :: c1, None :: c2 -> hjoint_col c1 c2
    | _ :: _, [] -> []
    | [], _ :: _ -> []
    | _ :: _, [_; _; _] -> []
    | [_; _; _], _ :: _ -> []
    | _ -> []

let rec place_col = function
    | [] -> []
    | Some k :: col -> body k :: place_col col
    | None :: col -> place_col col

let ortho mat =
    let rec f = function
        | [] -> []
        | [col] -> vjoint_col col @ place_col col
        | col1 :: col2 :: mat ->
            hjoint_col col1 col2 @ vjoint_col col1 @ place_col col1
            @ f (col2 :: mat)
    in
    f mat |> M.union

let pbody k =
    let w, d, _ = k.size in
    let top = k.f (w, d, 0.001) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let pfside k =
    let w, d, _ = k.size in
    let top = M.cube (w, 0.001, 0.001) |>> (0., d, 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let pnside k =
    let w, _, _ = k.size in
    let top = M.cube (w, 0.001, 0.001) |>> (0., 0., 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let prside k =
    let w, d, _ = k.size in
    let top = M.cube (0.001, d, 0.001) |>> (w, 0., 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let plside k =
    let _, d, _ = k.size in
    let top = M.cube (0.001, d, 0.001) |>> (0., 0., 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let pbarfl k =
    let _, d, _ = k.size in
    let top = M.cube (0.001, 0.001, 0.001) |>> (0., d, 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let pbarfr k =
    let w, d, _ = k.size in
    let top = M.cube (0.001, 0.001, 0.001) |>> (w, d, 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let pbarnl k =
    let top = M.cube (0.001, 0.001, 0.001) |>> (0., 0., 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let pbarnr k =
    let w, _, _ = k.size in
    let top = M.cube (0.001, 0.001, 0.001) |>> (w, 0., 0.) |@> k.a |>> k.p in
    let bottom = M.linear_extrude ~height:0.1 @@ M.projection top in
    M.hull [top; bottom]

let rec vjoint_proj = function
    | [] -> []
    | [_] -> []
    | None :: mat -> vjoint_proj mat
    | _ :: None :: mat -> vjoint_proj mat
    | Some k1 :: Some k2 :: mat ->
        M.hull [pfside k1; pnside k2] :: vjoint_proj (Some k2 :: mat)

let rec hjoint_proj c1 c2 =
    match (c1, c2) with
    | [], [] -> []
    | None :: c1, None :: c2 -> hjoint_proj c1 c2
    | [None], [Some _] -> []
    | [Some _], [None] -> []
    | [Some k1], [Some k2] -> [M.hull [prside k1; plside k2]]
    | Some k1 :: None :: c1, Some k2 :: None :: c2 ->
        M.hull [prside k1; plside k2] :: hjoint_proj c1 c2
    | Some k1 :: Some k1' :: mat1, Some k2 :: Some k2' :: mat2 ->
        M.hull [prside k1; plside k2]
        :: M.hull [pbarfr k1; pbarnr k1'; pbarfl k2; pbarnl k2']
        :: hjoint_proj (Some k1' :: mat1) (Some k2' :: mat2)
    | Some k1 :: Some k1' :: c1, Some k2 :: None :: c2 ->
        M.hull [prside k1; plside k2]
        :: M.hull [pbarfr k1; pbarnr k1'; pbarfl k2]
        :: hjoint_proj c1 c2
    | Some k1 :: None :: c1, Some k2 :: Some k2' :: c2 ->
        M.hull [prside k1; plside k2]
        :: M.hull [pbarfr k1; pbarnr k2'; pbarfr k2]
        :: hjoint_proj c1 c2
    | None :: c1, Some _ :: c2 -> hjoint_proj c1 c2
    | Some _ :: c1, None :: c2 -> hjoint_proj c1 c2
    | _ :: _, [] -> []
    | [], _ :: _ -> []
    | _ :: _, [_; _; _] -> []
    | [_; _; _], _ :: _ -> []
    | _ -> []

let rec place_proj = function
    | [] -> []
    | Some k :: col -> pbody k :: place_proj col
    | None :: col -> place_proj col

let proj mat =
    let rec f = function
        | [] -> []
        | [col] -> vjoint_proj col @ place_proj col
        | col1 :: col2 :: mat ->
            hjoint_proj col1 col2 @ vjoint_proj col1 @ place_proj col1
            @ f (col2 :: mat)
    in
    f mat |> M.union
