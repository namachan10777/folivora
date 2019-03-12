module Track = struct
    let pillar_d = 30.48

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

    let top_cutter = Model.cube (100., 100., 50.)
        |> Model.translate (-50., -50., 0.)

    let bottom_cutter = Model.cube (100., 100., 50.)
        |> Model.translate (-50., -50., -50.)

    let needle = Model.cylinder 0.0001 100.
        |> Model.translate (0., 0., -50.)

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

    let foundation_bottom = (pillar_d +. hole_region_r *. 2., ball_r *. 2. +. bearing_out_r +. bearing_shaft_h /. 2.)
    let foundation_center = (pillar_d /. 2. +. hole_region_r, pillar_d /. 2. +. hole_region_r)
    let bearinghedge_center tilt offset = (
        (fst foundation_center) -. offset *. (sin tilt),
        (snd foundation_center),
        ball_r -. offset *. (cos tilt))
    let top_surface_center tilt offset =
        Math.Pos.add (bearinghedge_center tilt offset) (
            bearing_shaft_r *. (sin tilt),
            0.,
            bearing_shaft_r *. (cos tilt)
        )

    let foundation_body tilt offset =
        let base = Model.cube (fst foundation_bottom, snd foundation_bottom, 40.) in
        Model.difference base [
            top_cutter
            |> Model.rotate (0., tilt, 0.)
            |> Model.translate (top_surface_center tilt offset)]

    let screw_far_p = (hole_region_r, snd foundation_center -. hole_r, 0.)
    let screw_near_p = ((fst foundation_bottom) -. hole_region_r, snd foundation_center -. hole_r, 0.)

    let screw_holes r =
        let points = [
            screw_far_p;
            screw_near_p;
        ] in
        let screw_holes = Model.cylinder ~fn:30 r 50. in
        Model.union @@ List.map (fun p -> Model.translate p screw_holes) points

    let mold tilt offset =
        let bearing_mold = Model.cylinder (bearing_out_r +. 3.0) (bearing_shaft_h +. 3.0)
            |> Model.translate (0., 0., -.(bearing_shaft_h +. 3.0) /. 2.)
            |> Model.rotate (0., pi /. 2., 0.) in
        Model.union [
            Model.minkowski [
                needle;
                arrange_as_circle bearing_mold offset
                |> Model.rotate (0., 0., pi)
                |> Model.rotate (0., tilt, 0.)
                |> Model.translate (bearinghedge_center tilt offset)];
            Model.cylinder (ball_r +. 4.0) 50.
            |> Model.translate (fst foundation_center, snd foundation_center, 0.);
            screw_holes 4.
        ]

    let foundation tilt offset =
        Model.intersection [
            Model.difference (foundation_body tilt offset) [
                bearing_circle offset
                |> Model.rotate (0., 0., pi)
                |> Model.rotate (0., tilt, 0.)
                |> Model.translate (bearinghedge_center tilt offset);
                Model.sphere (ball_r +. ball_c_foundation) ~fn:50
                |> Model.translate (fst foundation_center, snd foundation_center, ball_r);
                screw_holes 1.55;
                Model.cylinder 8.0 10.0 ~fn:30
                |> Model.translate (fst foundation_center, snd foundation_center, 0.0);
            ];
            mold tilt offset;
        ]

    let cover_top_surface_center tilt offset top_offset =
        Math.Pos.add (bearinghedge_center tilt offset) (
            (offset +. top_offset) *. (sin tilt),
            0.,
            (offset +. top_offset) *. (cos tilt)
        )

    let bearing_cover tilt offset top_offset = 
        let base = Model.cube (fst foundation_bottom, snd foundation_bottom, 100.) in
        let cover_center_z = match cover_top_surface_center tilt offset top_offset with (_, _, z) -> z in
        let cover_lowest_z = cover_center_z  -. (snd foundation_bottom) *. (sin tilt) /. 2. in
        (* 縁の部分の高さ *)
        let opening_r = sqrt (ball_r**2. -. top_offset**2.) in
        let cover_highest_z = cover_center_z +. opening_r *. (sin tilt) in
        let top_leveler =
            let cutter_p = (0., 0., cover_highest_z) in
            top_cutter |> Model.translate cutter_p in
        let sphere_hollwing =
            Model.minkowski [
                Model.sphere (ball_r +. ball_c_cover) ~fn:50;
                Model.cylinder 0.0001 10.0
            ]
            |> Model.translate (0., 0., -10.) in
        Model.intersection [
            Model.difference base [
                top_cutter
                |> Model.rotate (0., tilt, 0.)
                |> Model.translate (cover_top_surface_center tilt offset top_offset);
                bottom_cutter
                |> Model.rotate (0., tilt, 0.)
                |> Model.translate (top_surface_center tilt offset);
                sphere_hollwing
                |> Model.translate (fst foundation_center, snd foundation_center, ball_r);
                bearing_circle offset
                |> Model.rotate (0., 0., pi)
                |> Model.rotate (0., tilt, 0.)
                |> Model.translate (bearinghedge_center tilt offset);
                screw_holes 1.55;
                top_leveler;
                Model.cylinder 4. 10.0 ~fn:30 |> Model.translate (Math.Pos.add screw_near_p (0., 0., cover_lowest_z-.2.0));
                Model.cylinder 4. 10.0 ~fn:30 |> Model.translate (Math.Pos.add screw_far_p (0., 0., cover_highest_z-.2.0));
            ];
            mold tilt offset;
        ]

end
