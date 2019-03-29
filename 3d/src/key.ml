module Key = struct
    module M = Model
    module P = Math.Pos
    let key_wellhole_size = (15.5, 14.5, 1.2)
    let key_bottleneck_size = (14.0, 14.0, 1.0)
    let key_hollowing_size = (16.51, 16.51, 1.0)
    let key_block_size = (16.51, 21., (get_z key_wellhole_size) +. (get_z key_bottleneck_size) +. (get_z key_hollowing_size))

    let expand = function (x, y) -> (x, y, 0.0)

    let key_block =
        let half = 1. /. 2. in
        let key_block = M.cube key_block_size in
        let bottleneck = M.cube key_bottleneck_size in
        let hollowing = M.cube key_hollowing_size in
        let wellhole = M.cube key_wellhole_size in
        M.difference key_block [
            hollowing
            |>> ((key_block_size <-> key_hollowing_size) <*> (half, half, 0.0)
                <+> (0., 0., (get_z key_wellhole_size) +. (get_z key_bottleneck_size)));
            bottleneck
            |>> ((key_block_size <-> key_bottleneck_size) <*> (half, half, 0.0)
                <+> (0., 0., (get_z key_wellhole_size)));
            wellhole 
            |>> ((key_block_size <-> key_wellhole_size) <*> (half, half, 0.0));
        ]


    let bending = pi /. 10.

    let key_col near far =
        let rec place_rec p bend_total = function
            | 0 -> []
            | n ->
                let bend_total = bend_total +. bending in
                let succ_p = p <+> (0., (get_y key_block_size) *. (cos bend_total), (get_y key_block_size) *. (sin bend_total)) in
                (key_block |@> (bend_total, 0., 0.) |>> p) :: (place_rec succ_p bend_total (n - 1))
        in
        let f n = M.union (place_rec (0., get_y key_block_size, 0.) 0.0 n) in
        let far = f far in
        let near = f near |> M.mirror (0, 1, 0) |>> (0., get_y key_block_size, 0.) in
        M.union [key_block; far; near]

    (* 薄い側面を作って個別に凸包を取る *)
    let key_col_bridge_half p_diff left right =
        let plate = M.cube (0.01, get_y key_block_size, get_z key_block_size) in
        let pole = M.cube (0.01, 0.01, get_z key_block_size) in
        let rec place_rec p p_diff bend_total = function
            | (0, 0) -> []
            | (n, m) ->
                let bend_total = bend_total +. bending in
                let succ_p = p <+> (0., (get_y key_block_size) *. (cos bend_total), (get_y key_block_size) *. (sin bend_total)) in
                let place a b = M.hull [
                    (a |@> (bend_total, 0., 0.) |>> p);
                    (b |@> (bend_total, 0., 0.) |>> (p <+> p_diff));
                ] in
                match n, m with
                | 0, m -> (place pole plate) :: (place_rec succ_p p_diff bend_total (0, m-1))
                | n, 0 -> (place plate pole) :: (place_rec succ_p p_diff bend_total (n-1, 0))
                | n, m -> (place plate plate) :: (place_rec succ_p p_diff bend_total (n-1, m-1))
        in
        M.union (place_rec (0., get_y key_block_size, 0.) p_diff 0.0 (left, right))

    let key_col_bridge p_diff left right = match left, right with
        (left_near, left_far), (right_near, right_far) ->
            let plate = M.cube (0.01, get_y key_block_size, get_z key_block_size) in
            let far = key_col_bridge_half p_diff left_far right_far in
            let near = key_col_bridge_half (p_diff <*> (1., -1., 1.)) left_near right_near
                |> M.mirror (0, 1, 0) |>> (0., get_y key_block_size, 0.) in
            let base = M.hull [plate; plate |>> p_diff] in
            M.union [near; far; base]

    let key_pad l =
        let rec f x_acc = function
            | (near1, far1, p1) :: ((near2, far2, p2) as col2) :: tl ->
                let sub = P.sub p2 p1 in
                let sub = (get_x p2, get_y sub, get_z sub) in
                let bridge = key_col_bridge sub (near1, far1) (near2, far2) |>> (get_x key_block_size, 0., 0.) in
                let col = key_col near1 far1 in
                let p = (x_acc +. (get_x p1), get_y p1, get_z p1) in
                let x_acc = x_acc +. (get_x p1) +. (get_x key_block_size) in
                let col_set = M.union [col; bridge] |>> p  in
                col_set :: f x_acc (col2 :: tl)
            | (near, far, p) :: [] ->
                let col = key_col near far in
                [col |>> ((x_acc, 0., 0.) <+> p)]
            | [] -> []
        in M.union @@ f 0. l
    (* 左右側面: 左右に適当に延伸、延伸部分と底面への投影の間で各々凸法を取る *)
    (* 前後側面: 前後に水平に延伸、延伸部分と底面への投影の間で各々凸法を取る *)

    let rib_thin = 6.35

    let key_rib_side h near far =
        let rib_block = M.cube (rib_thin, get_y key_block_size, get_z key_block_size) in
        let rec place_rec p bend_total = function
            | 0 -> []
            | n ->
                let bend_total = bend_total +. bending in
                let succ_p = p <+> (0., (get_y key_block_size) *. (cos bend_total), (get_y key_block_size) *. (sin bend_total)) in
                let placed_block = (rib_block |@> (bend_total, 0., 0.) |>> p) in
                let projection = M.projection placed_block in
                let bottom = M.linear_extrude ~height:0.01 projection |>> (0., 0., -.h) in
                (M.hull [placed_block; bottom]):: (place_rec succ_p bend_total (n - 1))
        in
        let f n = M.union (place_rec (0., get_y key_block_size, 0.) 0.0 n) in
        let far = f far in
        let near = f near |> M.mirror (0, 1, 0) |>> (0., get_y key_block_size, 0.) in
        let orig = M.cube (rib_thin, get_y key_block_size, (get_z key_block_size) +. h) |>> (0., 0., -.h) in
        M.union [orig; far; near]

    let rec last = function
        | [] -> raise (Invalid_argument "")
        | x :: [] -> x
        | _ :: tl -> last tl

    let wall_h = 6.0

    let screw_holes h l =
        let x1 = (let (_, _, (x, _, _)) = (List.hd l) in x) -. rib_thin /. 2. in
        let x2 =
            (List.fold_left (fun acc (_, _, (x, _, _)) -> acc +. x) 0.0 l)
            +. rib_thin /. 2.
            +. (get_x key_block_size) *. (float_of_int (List.length l))in
        let y1 = 1.27 *. 24. in
        let y2 = -. 1.27 *. 12. in
        let screw_hole = M.union [
            M.cylinder 1.55 40. ~fn:30;
            M.cube (rib_thin, rib_thin, 40.) ~center:true |>> (0., 0., 20. +. h +. (get_z key_block_size));
        ] in
        M.union [
            screw_hole |>> (x1, y1, -.h);
            screw_hole |>> (x1, y2, -.h);
            screw_hole |>> (x2, y1, -.h);
            screw_hole |>> (x2, y2, -.h);
        ]


    let key_module l =
        let inside_rib = 
            l |> List.hd |> function (near, far, p) -> key_rib_side wall_h near far |>> (p <-> (rib_thin, 0., 0.)) in
        let outside_rib = 
            l |> last |> function (near, far, (_, y, z)) ->
                let x_acc = (List.length l |> float_of_int) *. (get_x key_block_size)
                +. List.fold_left (fun acc (_, _, p) -> acc +. (get_x p)) 0.0 l in
                key_rib_side wall_h near far |>> (x_acc, y, z) in
        M.difference
            (M.union [
                key_pad l;
                inside_rib;
                outside_rib;
            ])
            [screw_holes wall_h l]
end
