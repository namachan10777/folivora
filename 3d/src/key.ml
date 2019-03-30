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

    let col param =
        let (w, d, h) = key_block_size in
        let rec f p_acc b_acc = function
            | 0 -> []
            | n ->
                let b_acc = b_acc +. bending in
                let model = key_block |@> (b_acc, 0., 0.) |>> p_acc in
                let p_acc = p_acc <+> (0., d *. cos b_acc, d *. sin b_acc) in
                model :: f p_acc b_acc (n-1)
        in M.union @@ f (0., 0., 0.) 0. param

    let bond param p_diff =
        let plate = M.cube (0.0001, get_y key_block_size, get_z key_block_size) in
        let pole = M.cube (0.0001, 0.0001, get_z key_block_size) in
        let (w, d, h) = key_block_size in
        let rec f l_pole r_pole p_acc b_acc =
            let b_acc = b_acc +. bending in
            let model = plate |@> (b_acc, 0., 0.) |>> p_acc in
            let p_acc = p_acc <+> (0., d *. cos b_acc, d *. sin b_acc) in
            let renewed_l_pole = pole |@> (b_acc, 0., 0.) |>> p_acc in
            let renewed_r_pole = renewed_l_pole |>> p_diff in
            function
            | (0, 0) -> []
            | (n, 0) ->
                let bond = M.hull [ model; r_pole; ] in
                bond :: f renewed_l_pole r_pole p_acc b_acc (n-1, 0)
            | (0, m) ->
                let bond = M.hull [ l_pole; model |>> p_diff; ] in
                bond :: f l_pole renewed_r_pole p_acc b_acc (0, m-1)
            | (n, m) ->
                let bond = M.hull [ model; model |>> p_diff; ] in
                bond :: f renewed_l_pole renewed_r_pole p_acc b_acc (n-1, m-1)
        in M.union @@ f pole pole (0., 0., 0.) 0. param

    let key_pad params =
        let (w, d, h) = key_block_size in
        let rec arrange x_acc = function
            | (near, far, p) :: ((near', far', p') as param) :: tl ->
                let p_diff = p' <-> p <+> (p <*> (1., 0., 0.)) in
                let far_bond = bond (far, far') p_diff in
                let near_bond = bond (near, near') (p_diff <*> (1., -1., 1.)) in
                let near = col near in
                let far = col far in
                let plate = M.cube (0.001, get_y key_block_size, get_z key_block_size) in
                let center_bond = M.hull [plate; plate |>> p_diff ] in
                let col = M.union [
                    far |>> (0., get_y key_block_size, 0.);
                    far_bond |>> (get_x key_block_size, get_y key_block_size, 0.);
                    near |> M.mirror (0, 1, 0);
                    near_bond |>> (get_x key_block_size, 0., 0.) |> M.mirror (0, 1, 0);
                    key_block;
                    center_bond |>> (get_x key_block_size, 0., 0.);
                ] in
                (col |>> (p <+> (x_acc, 0., 0.))) :: arrange (x_acc +. w +. get_x p) (param :: tl)
            | (near, far, p) :: [] ->
                let far = col far in
                let near = col near in
                let col = M.union [
                    far |>> (0., get_y key_block_size, 0.);
                    near |> M.mirror (0, 1, 0);
                    key_block;
                ] in
                [col |>> (p <+> (x_acc, 0., 0.))]
            | [] -> []
        in M.union @@ arrange 0.0 params

    let rib_thin = 6.35

    let key_sidewall_half h n =
        let (_, d, _) = key_block_size in
        let wall_top = M.cube (rib_thin, get_y key_block_size, get_z key_block_size) in
        let rec f p_acc b_acc = function
            | 0 -> []
            | n ->
                let b_acc = b_acc +. bending in
                let wall_top = wall_top |@> (b_acc, 0., 0.) |>> p_acc in
                let bottom = wall_top |> M.projection |> M.linear_extrude ~height:0.0001 |>> (0., 0., -.h) in
                let p_acc = p_acc <+> (0., d *. cos b_acc, d *. sin b_acc) in
                (M.hull [wall_top; bottom]) :: f p_acc b_acc (n-1)
        in M.union @@ f (0., 0., 0.) 0. n

    let key_sidewall h (near, far) =
        let far = key_sidewall_half h far |>> (0., get_y key_block_size, 0.) in
        let near = key_sidewall_half h near |> M.mirror (0, 1, 0) in
        let base = M.cube (rib_thin, get_y key_block_size, h +. get_z key_block_size) |>> (0., 0., -.h) in
        M.union [far; near; base]

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
            l |> List.hd |> function (near, far, p) ->
                key_sidewall (wall_h -. get_z p) (near, far) |>> (p <-> (rib_thin, 0., 0.)) in
        let outside_rib = 
            l |> last |> function (near, far, (_, y, z)) ->
                let x_acc =
                    (List.length l |> float_of_int) *. (get_x key_block_size)
                    +. List.fold_left (fun acc (_, _, p) -> acc +. (get_x p)) 0.0 l in
                key_sidewall (wall_h -. z) (near, far) |>> (x_acc, y, z) in
        M.difference
            (M.union [
                key_pad l;
                inside_rib;
                outside_rib;
            ])
            [screw_holes wall_h l]
end
