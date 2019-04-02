module Track = struct
    module M = Model

    let ball_r = 17.0

    let ball_c_cover = 0.3
    let ball_c_foundation = 1.2

    let hole_r = 1.55
    let hole_c = 0.01
    let hole_region_r = 3.

    let bearing_shaft_r = 1.5
    let bearing_shaft_c = 0.1
    let bearing_shaft_h = 10.

    let bearing_in_r = 1.5
    let bearing_out_r = 3.25
    let bearing_c = 1.
    let bearing_t = 4.

    let eps = 0.1

    let pcb_thick = 1.8

    let offset = 5.0
    let bearing_arrange_r = sqrt ((ball_r +. bearing_out_r) ** 2. -. offset**2.)

    let bearing_hollowing =
        Model.union [
            (* ベアリングの軸 *)
            Model.cylinder (bearing_shaft_r +. bearing_shaft_c) bearing_shaft_h ~fn:30
                |@> (0., pi /. 2., 0.);
            (* ベアリング軸 (上から挿入出来るように切り抜き *)
            Model.cube (bearing_shaft_r +. bearing_shaft_c +.eps, (bearing_shaft_r +. bearing_shaft_c) *.2.,  bearing_shaft_h)
                |@> (0., pi /. 2., 0.)
                |>> (0., -.bearing_shaft_r-.bearing_shaft_c, bearing_shaft_r +. eps);
            (* ベアリング可動部 *)
            Model.cylinder (bearing_out_r+.bearing_c) bearing_t ~fn:30
                |@> (0., pi /. 2., 0.)
                |>> ((bearing_shaft_h -. bearing_t) /. 2., 0., 0.)
        ]
        (* center = true *)
        |>> (-.bearing_shaft_h/.2., 0., 0.)

    let arrange_as_circle model offset =
        let hole = model 
            |>> (0., bearing_arrange_r, 0.) in
        [
            hole;
            hole |@> (0., 0.,    pi *. 2. /. 3.);
            hole |@> (0., 0., -. pi *. 2. /. 3.)
        ]

    let bearing_circle =
        Model.union @@ arrange_as_circle bearing_hollowing offset

    let foundation_t = 5.0

    let trackball_block_w =
        let w1 = (bearing_arrange_r +. bearing_shaft_r) *. sin (pi /. 3.)
            +. bearing_shaft_h /. 2. *. cos (pi /. 3.) in
        let w2 = (bearing_arrange_r +. bearing_out_r +. bearing_c) *. sin (pi /. 3.)
            +. bearing_t /. 2. *. cos (pi /. 3.) in
        2. *. max w1 w2

    let trackball_block_d_near =
        let d1 = (bearing_arrange_r +. bearing_shaft_r) *. cos (pi /. 3.)
            +. bearing_shaft_h /. 2. *. sin (pi /. 3.) in
        let d2 = (bearing_arrange_r +. bearing_out_r +. bearing_c) *. cos (pi /. 3.)
            +. bearing_t /. 2. *. sin (pi /. 3.) in
        max (max d1 d2) ball_r

    let trackball_block_d_far =
        bearing_arrange_r +. bearing_out_r +. bearing_c

    let trackball_block =
        let under = Model.cube (trackball_block_w, trackball_block_d_far +. trackball_block_d_near, foundation_t) in
        let sphere = Model.sphere (ball_r +. ball_c_foundation) in
        Model.difference under [
            bearing_circle |>> (trackball_block_w /. 2., trackball_block_d_near, foundation_t -. bearing_shaft_r);
            sphere |>> (trackball_block_w /. 2., trackball_block_d_near, foundation_t -. bearing_shaft_r +. offset);
        ]

    let side_block_size = (6.35, 12.7, 10.0)

    let side_block =
        let (w, d, h) = side_block_size in
        let lump = M.cube side_block_size in
        let screw_hole = M.cylinder 1.55 10.0 ~fn:30 in
        Model.difference lump [
            screw_hole |>> (w /. 2., 2.54, 0.0);
            screw_hole |>> (w /. 2., d -. 2.54, 0.0);
        ]

    let side_plate = M.cube (0.001, get_y side_block_size, get_z side_block_size)
    let side_block_p1 = (-.40.64, -12.7, -5.0)
    let side_block_p2 = (+.25.4, 0.0, -5.0)

    let theta_step = pi /. 10.

    let thumb_keys =
        let (w, d, h) = Key.key_block_size in
        let key = Key.key_block in
        let key_plate = M.cube (0.001, d, h) in
        let rec arrange term p_acc bend_acc = function
            | 0 ->
                [M.hull [
                    key_plate |@> (0., 0., -.bend_acc) |>> p_acc;
                    side_plate |>> term;
                ]]
            | n ->
                let bond = M.hull [
                    key_plate |@> (0., 0., -.bend_acc) |>> p_acc;
                    key_plate |@> (0., 0., -.bend_acc -. theta_step) |>> p_acc;
                ] in
                let key = key |@> (0., 0., -.bend_acc -. theta_step) |>> p_acc in
                let bend_acc = bend_acc +. theta_step in
                let p_acc = p_acc <+> (w *. cos bend_acc, -. w *. sin bend_acc, 0.0) in
                bond :: key :: arrange term p_acc bend_acc (n-1)
        in
        let right = arrange (side_block_p2 <+> (w, 0., 0.)) (w, 0., 0.) 0. 1 |> M.union in
        let left = arrange (side_block_p1 <*> (-1.0, 1.0, 1.0)) (0., 0., 0.) 0. 2 |> M.union |> M.mirror (1, 0, 0) in
        let left_side_block = side_block |>> (side_block_p1 <-> (get_x side_block_size, 0., 0.)) in
        let right_side_block = side_block |>> (side_block_p2 <+> (w, 0., 0.)) in
        M.union [key; right; left; left_side_block; right_side_block]
end
