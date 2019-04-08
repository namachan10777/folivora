module Thumb = struct
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
    let lens_offset = 2.40
    let pcb_offset = 7.40 -. pcb_thick

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

    let foundation_t = 10.0

    let trackball_block_w =
        let w1 = (bearing_arrange_r +. bearing_shaft_r) *. sin (pi /. 3.)
            +. bearing_shaft_h /. 2. *. cos (pi /. 3.) in
        let w2 = (bearing_arrange_r +. bearing_out_r +. bearing_c) *. sin (pi /. 3.)
            +. bearing_t /. 2. *. cos (pi /. 3.) in
        2. *. max w1 w2

    let trackball_block_d_near =
        ball_r +. pcb_offset

    let trackball_block_d_far =
        bearing_arrange_r +. bearing_out_r +. bearing_c

    let lens_opening_size = 10.0
    let lens_hollowing_size = 20.

    let nut_insert =
        M.union [
            M.cylinder 1.55 20. ~fn:30 |@> (-.pi/.2., 0., 0.)
                |>> (0., 2.5 -. 20. +. 0.5, 0.);
            M.cylinder 3.2 2.5 ~fn:6 |@> (-.pi/.2., pi/.2., 0.);
            M.cube (5.5, 2.5, foundation_t /. 2.) |>> (-5.5/.2., 0., 0.);
        ]

    let nut_d = 15.24
    let screw_clearance = 2.
    let cover_max_t = offset +. 4.0
    let cover_min_t = 3.0

    let trackball_block =
        let w = trackball_block_w in
        let d = trackball_block_d_near +. trackball_block_d_far in
        let h = foundation_t +. cover_max_t in
        let lump = M.cube(w, d, h) in
        let sphere = M.sphere (ball_r +. ball_c_foundation) in
        let lens_opening = M.cube (lens_opening_size, ball_r, h) in
        let lens_hollowing = M.cube (lens_hollowing_size, pcb_offset -. pcb_thick, h) in
        let screw_hole = M.cylinder 1.55 h ~fn:30 in
        M.difference lump [
            bearing_circle |>> (w /. 2., trackball_block_d_near, foundation_t -. bearing_shaft_r);
            sphere |>> (trackball_block_w /. 2., trackball_block_d_near, foundation_t -. bearing_shaft_r +. offset);
            lens_opening |>> ((trackball_block_w -. lens_opening_size) /. 2., 0., 0.);
            lens_hollowing |>> ((trackball_block_w -. lens_hollowing_size) /. 2., 0., 0.);
            nut_insert |>> (trackball_block_w /. 2. -. nut_d, 2.4+.2.5, foundation_t/.2.);
            nut_insert |>> (trackball_block_w /. 2. +. nut_d, 2.4+.2.5, foundation_t/.2.);
            screw_hole |>> (screw_clearance, screw_clearance, 0.);
            screw_hole |>> (screw_clearance, trackball_block_d_near +. trackball_block_d_far -. screw_clearance, 0.);
            screw_hole |>> (trackball_block_w -. screw_clearance, trackball_block_d_near +. trackball_block_d_far -. screw_clearance, 0.);
            screw_hole |>> (trackball_block_w -. screw_clearance, screw_clearance, 0.);
        ]

    let trackball_foundation =
        M.difference trackball_block [
            M.cube (trackball_block_w, trackball_block_d_near +. trackball_block_d_far, cover_max_t)
            |>> (0., 0., foundation_t);
        ]

    let trackball_cover =
        let cover_slant = atan ((cover_max_t -. cover_min_t) /. trackball_block_w) in
        M.difference trackball_block [
            M.cube (trackball_block_w, trackball_block_d_near +. trackball_block_d_far, foundation_t)
            |>> (0., 0., 0.);
            M.cube (trackball_block_w +. 10.0, trackball_block_d_near +. trackball_block_d_far, cover_max_t +. 10.)
            |@> (0., -.cover_slant, 0.)
            |>> (0., 0., foundation_t +. cover_min_t);
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
    let side_block_p1 = (-.44.45, -12.7, -5.0)
    let side_block_p2 = (+.40.64, 0.0, -5.0)

    let theta_step = pi /. 10.

    let thumb_key_w = 18.0
    let thumb_key_block_size = (thumb_key_w, get_y Key.key_block_size, get_z Key.key_block_size)

    let thumb_key =
        let lump = M.cube thumb_key_block_size in
        Model.difference lump [
            Key.key_hollowing |>> (thumb_key_block_size <*> (1./.2., 1./.2., 0.0));
        ]

    let sidewall_t = 3.0
    let sidewall_h = 5.0 +. (get_z thumb_key_block_size)

    let thumb_keys =
        let (w, d, h) = thumb_key_block_size in
        let key = thumb_key in
        let key_plate = M.cube (0.001, d, h) in
        let rec arrange term p_acc bend_acc = function
            | 0 ->
                let key_plate = M.cube (0.001, d +. sidewall_t, h) |>> (0., -.sidewall_t, 0.) in
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
                let sidewall = M.cube (get_x thumb_key_block_size, sidewall_t, sidewall_h)
                    |>> (0., -.sidewall_t, 0.)
                    |@> (0., 0., -.bend_acc -. theta_step)
                    |>> (p_acc <+> (0., 0., (get_z thumb_key_block_size) -. sidewall_h)) in
                let bend_acc = bend_acc +. theta_step in
                let p_acc = p_acc <+> (w *. cos bend_acc, -. w *. sin bend_acc, 0.0) in
                bond :: sidewall :: key :: arrange term p_acc bend_acc (n-1)
        in
        let right = arrange (side_block_p2 <+> (0., 0., 0.)) (w, 0., 0.) 0. 1 |> M.union in
        let left = arrange (side_block_p1 <*> (-1.0, 1.0, 1.0)) (0., 0., 0.) 0. 2 |> M.union |> M.mirror (1, 0, 0) in
        let left_side_block = side_block |>> (side_block_p1 <-> (get_x side_block_size, 0., 0.)) in
        let right_side_block = side_block |>> (side_block_p2) in
        let sidewall = M.cube(get_x thumb_key_block_size, sidewall_t, sidewall_h)
            |>> (0., -.sidewall_t, (get_z thumb_key_block_size) -. sidewall_h) in
        M.union [key; right; left; left_side_block; right_side_block; sidewall]

    let trackball_p = (-10., -27., ball_r -. offset -. foundation_t -. (get_z side_block_size) -. (get_z side_block_p1))
    let thumbkey_p = (-32., -30., 0.)

    let thumb_track =
        let (w, d, h) = thumb_key_block_size in
        let d = 35.0 in
        let thumb_key_block_size = (w, d, h) in
        let thumb_key = M.difference (M.cube thumb_key_block_size) [
            Key.key_hollowing |>> (thumb_key_block_size <*> (1./.2., 0.0, 0.0) <+> (0., 15., 0.));
        ] in
        let left_side_block = side_block |>> (side_block_p1 <-> (get_x side_block_size, 0., 0.)) in
        let right_side_block = side_block |>> side_block_p2 in
        let trackball_block = trackball_foundation |>> trackball_p in
        let key_plate = M.cube (0.001, d, h) in
        let trackball_plate = M.cube (0.0001, trackball_block_d_far+.trackball_block_d_near, foundation_t) in
        let key = thumb_key |@> (0., 0., pi/.10.) |>> thumbkey_p in
        let bonds = [
            M.hull [
                key_plate |@> (0., 0., pi/.10.) |>> thumbkey_p;
                side_plate |>> side_block_p1;
            ];
            M.hull [
                key_plate |>> (get_x thumb_key_block_size, 0., 0.) |@> (0., 0., pi/.10.) |>> thumbkey_p;
                trackball_plate |>> trackball_p;
            ];
            M.hull [
                trackball_plate |>> (trackball_p <+> (trackball_block_w, 0., 0.));
                side_plate |>> side_block_p2;
            ];
        ] in
        M.union @@ [left_side_block;right_side_block; trackball_block; key;] @ bonds
end
