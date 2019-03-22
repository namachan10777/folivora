module Track = struct
    let pillar_d_w = 2.54 *. 16.
    let pillar_d_d = 2.54 *. 6.

    let ball_r = 12.5

    let ball_c_cover = 0.3
    let ball_c_foundation = 1.2

    let hole_r = 1.55
    let hole_c = 0.01
    let hole_region_r = 3.

    let bearing_shaft_r = 1.5
    let bearing_shaft_c = 0.1
    let bearing_shaft_h = 15.

    let bearing_in_r = 1.5
    let bearing_out_r = 3.25
    let bearing_c = 1.
    let bearing_t = 5.

    let eps = 0.1

    let pcb_thick = 1.8

    let top_cutter = Model.cube (100., 100., 50.)
        |> Model.translate (-50., -50., 0.)

    let bottom_cutter = Model.cube (100., 100., 50.)
        |> Model.translate (-50., -50., -50.)

    let needle = Model.cylinder 0.0001 50.
        |> Model.translate (0., 0., -10.)

    let bearing_hollowing =
        Model.union [
            (* ベアリングの軸 *)
            Model.cylinder (bearing_shaft_r +. bearing_shaft_c) bearing_shaft_h ~fn:30
                |> Model.rotate (0., pi /. 2., 0.);
            (* ベアリング軸 (上から挿入出来るように切り抜き *)
            Model.cube (bearing_shaft_r +. bearing_shaft_c +.eps, (bearing_shaft_r +. bearing_shaft_c) *.2.,  bearing_shaft_h)
                |> Model.rotate (0., pi /. 2., 0.)
                |> Model.translate (0., -.bearing_shaft_r-.bearing_shaft_c, bearing_shaft_r +. eps);
            (* ベアリング可動部 *)
            Model.cylinder (bearing_out_r+.bearing_c) bearing_t ~fn:30
                |> Model.rotate (0., pi /. 2., 0.)
                |> Model.translate ((bearing_shaft_h -. bearing_t) /. 2., 0., 0.)
        ]
        (* center = true *)
        |> Model.translate (-.bearing_shaft_h/.2., 0., 0.)

    let arrange_as_circle model offset =
        let r = sqrt ((ball_r +. bearing_out_r) ** 2. -. offset**2.) in
        let hole = model 
            |> Model.translate (0., r, 0.) in
        Model.union [
            hole;
            hole |> Model.rotate (0., 0.,    pi *. 2. /. 3.);
            hole |> Model.rotate (0., 0., -. pi *. 2. /. 3.)
        ]

    let bearing_circle offset =
        arrange_as_circle bearing_hollowing offset

    let lens_len = 15.6
    let nut_insert_d = 2.54 *. 12.
    let lens_ball_offset = 2.40
    let ball_optpcb_offset = 7.40
    let pcb_t = 1.8
    let lens_hollowing_depth = ball_optpcb_offset -. pcb_t
    let depth_first_half = ball_r +. lens_hollowing_depth

    let block_size southern nothern =
        let width = pillar_d_w +. hole_region_r *. 2. in
        let depth =
            (* 奥側半分 *)
            (pillar_d_d +. hole_region_r) +.
            (* 手前側半分 *)
            (ball_r +. 7.40) in
        let height = southern +. nothern in
        (width, depth, height)

    let screw_poss southern nothern = 
        let (width, depth, _) as block_size = block_size southern nothern in
        let xs = [hole_region_r; width -. hole_region_r] in
        let ys = [depth_first_half; depth_first_half +. pillar_d_d] in
        List.flatten @@ List.map (fun x -> List.map (fun y -> (x, y, 0.)) ys) xs

    let screw_poss_rel offset southern nothern =
        let (width, depth, _) as block_size = block_size southern nothern in
        screw_poss southern nothern |> List.map (fun p -> Math.Pos.sub p (width/.2., depth/.2., southern -. bearing_shaft_r +. offset))

    let trackball_block offset nothern southern =
        let (width, depth, height) as block_size = block_size southern nothern in
        let lump = Model.cube block_size in
        let bearings = bearing_circle offset in
        let sphere = Model.sphere (ball_r +. ball_c_foundation) ~fn:100 in
        let screw_hole = Model.cylinder hole_r height ~fn:30 in
        let lens_cut = Model.cube (lens_len, lens_hollowing_depth, height) in
        let lens_window = Model.cube (12.96 -. 0.5, depth_first_half, height) in
        let screw_holes =
            let ps = screw_poss southern nothern in
            Model.union (List.map (fun p -> Model.translate p screw_hole) ps) in
        Model.difference lump [
            bearings |> Model.translate (width/.2., depth_first_half, southern -. bearing_shaft_r);
            sphere |> Model.translate (width/.2., depth_first_half, southern -. bearing_shaft_r +. offset);
            screw_holes;
            lens_cut |> Model.translate ((width -. lens_len) /. 2., 0., 0.);
            lens_window |> Model.translate ((width -. (12.96 -. 0.5)) /. 2., 0., 0.);
        ]

    let foundation offset =
        let height = (bearing_shaft_r +. 3.0) in
        let (width, _, _)  = block_size 0.0 height in
        let lump = trackball_block offset 0.0 height in
        let nut = Model.cylinder 3.2 2.4 ~fn:6 in
        let nut_insert = Model.minkowski [
            nut |> Model.rotate (pi/.2., 0., 0.) |> Model.translate (0., 2.4, 0.);
            Model.cylinder 0.0001 (height /. 2.);
        ] in
        let opt_screw = Model.cylinder hole_r 10. ~fn:30 |> Model.translate (0., 0., -5.) |> Model.rotate (pi/.2., 0., 0.) in
        let nut_hollowing = Model.union [
            nut_insert;
            opt_screw;
        ] in
        let nut_inserts =
            let xs = [-.nut_insert_d /. 2.; nut_insert_d /. 2.] in
            Model.union @@ List.map (fun x -> Model.translate (x, 0., height/.2.) nut_hollowing) xs in
        Model.difference lump [
            nut_inserts |> Model.translate (width /. 2., 3., 0.);
        ]

    let rim_height = 3.0

    let cover offset ledge =
        let height = offset +. rim_height in
        let block_size = block_size 0.0 pcb_thick in
        let cutter = Model.cube block_size in
        let lump = trackball_block offset height pcb_thick in
        Model.difference lump [
            cutter |> Model.translate (0., ledge, 0.);
        ]

    let screw_positions offset =
        screw_poss_rel offset 0.0 (bearing_shaft_r +. 3.0)
end
