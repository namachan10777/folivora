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

    let key_pad params =
        let (w, d, h) = key_block_size in
        let rec build x_acc =
            function
            | (n, dy, dz) :: ((n', dy', dz') as succ) :: tl ->
                let near = M.union [
                    key_block;
                    key_block |>> (0., d, 0.);
                ] in
                let far = col n |>> (0., 2. *. d, 0.) in
                let bond = bond (n, n') (dy' -. dy, dz' -. dz) |>> (w, 2. *. d, 0.) in
                let bond' =
                    let side_face = M.cube (0.001, 2. *. d, h) in
                    M.hull [
                        side_face |>> (w, 0., 0.);
                        side_face |>> (w +. col_d, dy' -. dy, dz' -. dz);
                    ] in
                let packed = M.union [ near; far; bond; bond' ] |>> (x_acc, dy, dz) in
                packed :: build (x_acc +. w +. col_d) (succ :: tl)
            | (n, dy, dz) :: [] ->
                let near = M.union [
                    key_block;
                    key_block |>> (0., d, 0.);
                ] in
                let far = col n |>> (0., 2. *. d, 0.) in
                [M.union [ near; far ] |>> (x_acc, dy, dz)]
            | [] -> []
        in M.union @@ build 0.0 params
end
