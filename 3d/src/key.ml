module Key = struct
    module M = Model
    module P = Math.Pos
    let key_wellhole_d = 3.5
    let key_wellhole_size = (15.5, 4.0, 1.8)
    let key_hollowing_size = (15.5, 15.5, 1.2)
    let key_bottleneck_size = (14.0, 14.0, 1.0 +. get_z key_wellhole_size)
    let key_block_size = (16.51, 20., (get_z key_bottleneck_size) +. (get_z key_hollowing_size))

    let expand = function (x, y) -> (x, y, 0.0)

    let key_hollowing =
        let centerize = (-1./.2., -1./.2., 0.0) in
        let bottleneck = M.cube key_bottleneck_size |>> (key_bottleneck_size <*>  centerize) in
        let hollowing = M.cube key_hollowing_size |>> (key_hollowing_size <*> centerize) in
        let wellhole = M.cube key_wellhole_size |>> (key_wellhole_size <*> centerize) in
        M.union [
            bottleneck;
            hollowing |>> (0., 0., get_z key_bottleneck_size);
            wellhole |>> (0., -.key_wellhole_d, 0.);
            wellhole |>> (0., +.key_wellhole_d, 0.);
        ]

    let key_block =
        let key_block = M.cube key_block_size in
        M.difference key_block [
            key_hollowing |>> ((get_x key_block_size) /. 2., (get_y key_block_size) /. 2., 0.);
        ]


    let bending = pi /. 10.

    let col_d = 2.54

    let col_impl block param =
        let (w, d, h) = key_block_size in
        let rec f p_acc b_acc = function
            | 0 -> []
            | n ->
                let b_acc = b_acc +. bending in
                let model = key_block |@> (b_acc, 0., 0.) |>> p_acc in
                let p_acc = p_acc <+> (0., d *. cos b_acc, d *. sin b_acc) in
                model :: f p_acc b_acc (n-1)
        in M.union @@ f (0., 0., 0.) 0. param

    let col = col_impl key_block

    let wall_t = 3.0
    let wall_clearance = 3.0

    let col_side_wall param wall_h_base =
        let (w, d, h) = key_block_size in
        let rec f p_acc b_acc = function
            | 0 -> []
            | n ->
                let b_acc = b_acc +. bending in
                let model = M.cube (wall_t, d, h) |@> (b_acc, 0., 0.) |>> p_acc in
                let bottom = M.cube (wall_t, d *. cos b_acc, 0.001) |>> (get_x p_acc, get_y p_acc, -.wall_h_base) in
                let p_acc = p_acc <+> (0., d *. cos b_acc, d *. sin b_acc) in
                (M.hull [ model; bottom ]) :: f p_acc b_acc (n-1)
        in M.union @@ f (0., 0., 0.) 0. param

    let col_side_wall_with_ext param wall_h_base =
        let (w, d, h) = key_block_size in
        let pos = List.init param (fun n -> bending +. bending *. float_of_int n)
            |> List.fold_left (fun p bend -> p <+> (0., d *. cos bend, d *. sin bend)) (0., 0., 0.) in
        M.union [
            col_side_wall param wall_h_base;
            M.cube (
                wall_t,
                wall_t +. wall_clearance +. h *. sin (bending *. float_of_int param),
                wall_h_base +. (get_z pos) +. h *. cos (bending *. float_of_int param))
            |>> (0., (get_y pos) -. h *. sin (bending *. float_of_int param), -.wall_h_base);
        ]

    let col_wall param wall_h_base =
        let (w, d, h) = key_block_size in
        let pos = List.init param (fun n -> bending +. bending *. float_of_int n)
            |> List.fold_left (fun p bend -> p <+> (0., d *. cos bend, d *. sin bend)) (0., 0., 0.) in
        let thick = h *. cos (bending *. float_of_int param) in
        let ext = M.hull [
            M.cube (w, 0.001, h) |@> (bending *. float_of_int param, 0., 0.) |>> pos;
            M.cube (w, wall_clearance, h *. cos (bending *. float_of_int param)) |>> pos;
        ] in
        let wall_h = wall_h_base +. (get_z pos) +. h *. cos (bending *. float_of_int param) in
        let wall = M.cube (w, wall_t, wall_h) |>> (0., wall_clearance +. get_y pos, -.wall_h_base) in
        M.union [ ext; wall ]

    let bond_wall wall_h_base (ln, rn) (dy, dz) =
        let (w, d, h) = key_block_size in
        let pos n = List.init n (fun n -> bending +. bending *. float_of_int n)
            |> List.fold_left (fun p bend -> p <+> (0., d *. cos bend, d *. sin bend)) (0., 0., 0.) in
        let lpos = pos ln in
        let rpos = (pos rn) <+> (col_d, dy, dz) in
        let addtional_ext = if ln > rn then [
            M.cube (0.001, 0.001, h) |@> (bending *. float_of_int rn, 0., 0.) |>> (rpos <+> (wall_t, 0., 0.));
            M.cube (0.001, wall_clearance, h *. cos (bending *. float_of_int rn)) |>> (rpos <+> (wall_t, 0., 0.));
        ] else if rn > ln then [
            M.cube (0.001, 0.001, h) |@> (bending *. float_of_int ln, 0., 0.) |>> (lpos <-> (wall_t, 0., 0.));
            M.cube (0.001, wall_clearance, h *. cos (bending *. float_of_int ln)) |>> (lpos <-> (wall_t, 0., 0.));
        ] else [] in
        let bond_ext = M.hull (addtional_ext @ [
            M.cube (0.001, 0.001, h) |@> (bending *. float_of_int ln, 0., 0.) |>> lpos;
            M.cube (0.001, wall_clearance, h *. cos (bending *. float_of_int ln)) |>> lpos;
            M.cube (0.001, 0.001, h) |@> (bending *. float_of_int rn, 0., 0.) |>> rpos;
            M.cube (0.001, wall_clearance, h *. cos (bending *. float_of_int rn)) |>> rpos;
        ]) in
        let wall_h_l = wall_h_base +. (get_z lpos) +. h *. cos (bending *. float_of_int ln) in
        let wall_h_r = wall_h_base +. (get_z rpos) +. h *. cos (bending *. float_of_int rn) in
        let addtional_wall = if ln > rn then [
            M.cube (0.001, wall_t, wall_h_r) |>> (col_d +. wall_t, wall_clearance +. get_y rpos, -.wall_h_base);
        ] else if rn > ln then [
            M.cube (0.001, wall_t, wall_h_l) |>> (-.wall_t, wall_clearance +. get_y lpos, -.wall_h_base);
        ] else [] in
        let bond_wall = M.hull (addtional_wall @ [
            M.cube (0.001, wall_t, wall_h_l) |>> (0., wall_clearance +. get_y lpos, -.wall_h_base);
            M.cube (0.001, wall_t, wall_h_r) |>> (col_d, wall_clearance +. get_y rpos, -.wall_h_base);
        ]) in
        M.union [ bond_ext; bond_wall ]

    let bond col_num (dy, dz) =
        let (w, d, h) = key_block_size in
        let plate = M.cube (0.0001, d, h) in
        let pole = M.cube (0.0001, 0.0001, h) in
        let rec f l_pole r_pole p_acc b_acc =
            let b_acc = b_acc +. bending in
            let side_face = plate |@> (b_acc, 0., 0.) |>> p_acc in
            let p_acc = p_acc <+> (0., d *. cos b_acc, d *. sin b_acc) in
            (* 基準の位置に移動 *)
            let renewed_l_pole = pole |@> (b_acc, 0., 0.) |>> p_acc in
            (* 左側を基準に右側も動かす *)
            let renewed_r_pole = renewed_l_pole |>> (col_d, dy, dz) in
            function
            | (0, 0) -> []
            | (n, 0) ->
                let bond = M.hull [ side_face; r_pole; ] in
                (* 右側は既に終了しているので更新しない *)
                bond :: f renewed_l_pole r_pole p_acc b_acc (n-1, 0)
            | (0, m) ->
                let bond = M.hull [ l_pole; side_face |>> (col_d, dy, dz); ] in
                (* 左側は既に終了しているので更新しない *)
                bond :: f l_pole renewed_r_pole p_acc b_acc (0, m-1)
            | (n, m) ->
                let bond = M.hull [ side_face; side_face |>> (col_d, dy, dz); ] in
                bond :: f renewed_l_pole renewed_r_pole p_acc b_acc (n-1, m-1)
        in M.union @@ f pole pole (0., 0., 0.) 0. col_num

    let wall_h = 5.0

    let key_pad params =
        let (w, d, h) = key_block_size in
        let rec build x_acc =
            function
            | (n, dy, dz) :: ((n', dy', dz') as succ) :: tl ->
                let middle = key_block in
                let near = col 1 |> M.mirror (0, 1, 0) in
                let far = col n |>> (0., d, 0.) in
                let far_wall = col_wall n (wall_h +. dz) |>> (0., d, 0.) in
                let bond_far = bond (n, n') (dy' -. dy, dz' -. dz) |>> (w, d, 0.) in
                let bond_wall_far = bond_wall (wall_h +. dz) (n, n') (dy' -. dy, dz' -. dz) |>> (w, d, 0.) in
                let bond_near = bond (1, 1) (dy -. dy', dz' -. dz) |> M.mirror (0, 1, 0) |>> (w, 0., 0.) in
                let bond =
                    let side_face = M.cube (0.001, d, h) in
                    M.hull [
                        side_face |>> (w, 0., 0.);
                        side_face |>> (w +. col_d, dy' -. dy, dz' -. dz);
                    ] in
                let packed = M.union [ near; middle; far; bond_near; bond; bond_far; far_wall; bond_wall_far ] |>> (x_acc, dy, dz) in
                packed :: build (x_acc +. w +. col_d) (succ :: tl)
            | (n, dy, dz) :: [] ->
                let middle = key_block in
                let near = col 1 |> M.mirror (0, 1, 0) in
                let far = col n |>> (0., d, 0.) in
                let far_wall = col_wall n (wall_h +. dz) |>> (0., d, 0.) in
                [M.union [ near; middle; far; far_wall ] |>> (x_acc, dy, dz)]
            | [] -> [] in
        let key_pad = M.union @@ build 0.0 params in
        match params with
        | (_, dy, dz) :: _ -> key_pad |>> (0., -.dy +. d *. cos bending, -.dz -. d *. sin bending)
        | [] -> key_pad

    let thumb_key_curve = pi /. 8.

    let thumb_key_block =
        let (w, d, h) = key_block_size in
        M.difference (M.cube (19.05, d, h)) [
            key_hollowing |>> (19.05 /. 2., d/.2., 0.);
        ]

    let thumb_p =
        let (_, d, _) = key_block_size in
        (19.05, -19.05, -.d *. sin bending)

    let rec last = function
        | [] -> raise (Invalid_argument "")
        | [x] -> x
        | x :: tl -> last tl

    let key_main params =
        let (_, oy, oz) = List.hd params in
        let orig = (0., oy, oz) in
        let (w, d, h) = key_block_size in
        let w' = 19.05 in
        let pad = key_pad params in
        let needle = M.cube(0.001, 0.001, h) in
        let key_side_face = M.cube (0.001, d, h) in
        let thumb_key_block = M.union [
            thumb_key_block;
            M.cube (w', wall_t, wall_h +. h) |>> (0., -.wall_t, -.wall_h);
        ] in
        let thumb4 =
            M.union [
                thumb_key_block;
                M.cube (wall_t, d +. wall_t, wall_h +. h) |>> (w', -. wall_t, -.wall_h);
            ] |@> (0., 0., -.thumb_key_curve) |>> (thumb_p <+> (w', 0., 0.)) in
        let thumb3 = thumb_key_block |>> thumb_p in
        let thumb2 = thumb_key_block |>> (-.w', 0., 0.) |@> (0., 0., thumb_key_curve) |>> thumb_p in
        let thumb1 =
            M.union [
                thumb_key_block;
                M.cube (wall_t, d +. wall_t, wall_h +. h) |>> (-.wall_t, -. wall_t, -.wall_h);
                M.cube (wall_t +. w', wall_t, wall_h +. h) |>> (-.wall_t, d, -.wall_h);
            ]
            |>> (-.w', 0., 0.)
            |@> (0., 0., 2. *. thumb_key_curve)
            |>> (thumb_p <-> (w' *. cos thumb_key_curve, w' *. sin thumb_key_curve, 0.))  in
        let bond1 = M.hull [
            key_side_face |@> (0., 0., 2. *. thumb_key_curve) |>> (-.w' *. cos thumb_key_curve, -.w' *. sin thumb_key_curve, 0.);
            key_side_face |@> (0., 0., 1. *. thumb_key_curve) |>> (-.w' *. cos thumb_key_curve, -.w' *. sin thumb_key_curve, 0.);
        ] |>> thumb_p in
        let bond2 = M.hull [
            key_side_face;
            key_side_face |@> (0., 0., thumb_key_curve);
        ] |>> thumb_p in
        let bond3 = M.hull [
            key_side_face;
            key_side_face |@> (0., 0., -.thumb_key_curve);
        ] |>> ((w', 0., 0.) <+> thumb_p) in
        let ext0 = M.hull [
            M.cube (w', 0.001, h)
            |>> (-.w', d, 0.)
            |@> (0., 0., thumb_key_curve)
            |>> thumb_p;
            M.cube (w, 0.001, h)
            |@> (-.bending, 0., 0.);
        ] in
        let make_ext_to_pad_bond n parts =
            let (_, dy, dz) = List.nth params n in
            let (_, dy', dz') = List.nth params (n+1) in
            M.hull ([
                needle
                |@> (-.bending, 0., 0.)
                |>> ((w +. (col_d *. float_of_int n) +. w *. float_of_int n, dy, dz) <-> orig);
                needle
                |@> (-.bending, 0., 0.)
                |>> ((w +. col_d +. (col_d *. float_of_int n) +. w *. float_of_int n, dy', dz') <-> orig);
            ] @ parts) in
        let make_ext_to_pad_key n parts =
            let (_, dy, dz) = List.nth params n in
            M.hull ([
                M.cube (w, 0.001, h)
                |@> (-.bending, 0., 0.)
                |>> (((w +. col_d) *. float_of_int n, dy, dz) <-> orig);
            ] @ parts) in
        let ext1 = make_ext_to_pad_bond 0 [
                needle |>> (0., d, 0.) |@> (0., 0., thumb_key_curve) |>> thumb_p;
                needle |>> ((0., d, 0.) <+> thumb_p);
            ] in
        let ext2 = make_ext_to_pad_key 1 [
                M.cube (w', 0.001, h) |>> (thumb_p <+> (0., d, 0.));
            ] in
        let ext3 = make_ext_to_pad_bond 1 [
                needle |>> (0., d, 0.) |@> (0., 0., -.thumb_key_curve) |>> (thumb_p <+> (w', 0., 0.));
                needle |>> ((w', d, 0.) <+> thumb_p);
            ] in
        let ext4 = make_ext_to_pad_key 2 [
                M.cube (w', 0.001, h) |>> (0., d, 0.) |@> (0., 0., -.thumb_key_curve) |>> ((w', 0., 0.) <+> thumb_p);
            ] in
        let ext5 = make_ext_to_pad_bond 2 [
                M.cube (w', 0.001, h) |>> (0., d, 0.) |@> (0., 0., -.thumb_key_curve) |>> ((w', 0., 0.) <+> thumb_p);
            ] in
        let ext6 = make_ext_to_pad_key 3 [
                M.cube (w', 0.001, h) |>> (0., d, 0.) |@> (0., 0., -.thumb_key_curve) |>> ((w', 0., 0.) <+> thumb_p);
            ] in
        let ext7 = make_ext_to_pad_bond 3 [
                needle |>> (w', d, 0.) |@> (0., 0., -.thumb_key_curve) |>> ((w', 0., 0.) <+> thumb_p);
            ] in
        let side_wall_l =
            let side_wall2_with_ext = col_side_wall_with_ext 2 wall_h in
            let side_wall1 = col_side_wall 1 wall_h in
            M.union [
                side_wall2_with_ext |>> (0., d, 0.);
                side_wall1 |> M.mirror (0, 1, 0);
                M.cube (wall_t, d, wall_h +. h) |>> (0., 0., -.wall_h);
            ] |>> (-.wall_t, d *. cos bending, -. d *. sin bending) in
        let side_wall_r =
            let x = ((col_d +. w) *. float_of_int (List.length params)) -. col_d in
            let (_, dy, dz) = last params in
            let side_wall1_with_ext = col_side_wall_with_ext 1 (wall_h +. dz) in
            let side_wall1 = col_side_wall 1 (wall_h +. dz) in
            M.union [
                side_wall1_with_ext |>> (0., d, 0.);
                side_wall1_with_ext |> M.mirror (0, 1, 0);
                side_wall1 |> M.mirror (0, 1, 0);
                M.cube (wall_t, d, wall_h +. h +. dz) |>> (0., 0., -.wall_h -. dz);
            ] |>> (x, dy +. d *. cos bending, dz -. d *. sin bending) in
        let wall_near = 
            let (n, dy, dz) = List.nth params 4 in
            let (n', dy', dz') = List.nth params 5 in
            M.union [
                col_wall 1 (wall_h +. dz);
                col_wall 1 (wall_h +. dz') |>> (w +. col_d, dy -. dy', dz' -. dz);
                bond_wall (wall_h +. dz) (1, 1) (dy -. dy', dz' -. dz) |>> (w, 0., 0.);
            ] |> M.mirror (0, 1, 0) |>> ((w +. col_d) *. 4., dy +. d *. cos bending, dz -. d *. sin bending) in
        let ext8 =
            let (_, dy, dz) = List.nth params 4 in
            M.hull [
                M.hull [
                    M.cube (0.001, 0.001, wall_h +. h) |>> (w', d, -.wall_h);
                    M.cube (0.001, 0.001, wall_h +. h) |>> (w' +. wall_t, d, -.wall_h);
                ] |@> (0., 0., -.thumb_key_curve) |>> (thumb_p <+> (w', 0., 0.));
                M.hull [
                    M.cube (0.001, wall_t, 0.001);
                    M.cube (0.001, 0.001, dz +. wall_h +. (d *. sin bending) +. (h *. cos bending));
                    M.cube (0.001, wall_t +. wall_clearance +. h *. sin bending, 0.001)
                    |>> (0., 0., dz +. wall_h +. (d *. sin bending) +. (h *. cos bending));
                ] |>> ((w +. col_d) *. 4., dy -. wall_clearance -. wall_t, -. wall_h -. d *. sin bending);
            ] in
        let ext9 = M.hull [
            M.cube (0.001, wall_t, wall_h +. h)
            |>> (0., d, 0.)
            |@> (0., 0., 2. *. thumb_key_curve)
            |>> ((-. w' *. cos thumb_key_curve, -.w' *. sin thumb_key_curve, -.wall_h) <+> thumb_p);
            M.cube (0.001, wall_t, wall_h +. h)
            |>> (0., d, 0.)
            |@> (0., 0., thumb_key_curve)
            |>> ((-. w' *. cos thumb_key_curve, -.w' *. sin thumb_key_curve, -.wall_h) <+> thumb_p);
        ] in
        let ext10 = M.hull [
            M.cube (0.001, wall_t, wall_h +. h)
            |>> (0., d, 0.)
            |@> (0., 0., thumb_key_curve)
            |>> ((-. w' *. cos thumb_key_curve, -.w' *. sin thumb_key_curve, -.wall_h) <+> thumb_p);
            M.cube (wall_t, 0.001, wall_h +. d *. sin bending)
            |>> (-.wall_t, 0., -.wall_h -. d *. sin bending);
            M.cube (wall_t, 0.001, h)
            |@> (-.bending, 0., 0.)
            |>> (-.wall_t, 0., 0.);
        ] in
        M.union [
            wall_near;
            side_wall_l;
            side_wall_r;
            pad;
            thumb1;
            thumb2;
            thumb3;
            thumb4;
            bond1;
            bond2;
            bond3;
            ext0;
            ext1;
            ext2;
            ext3;
            ext4;
            ext5;
            ext6;
            ext7;
            ext8;
            ext9;
            ext10;
        ]
end
