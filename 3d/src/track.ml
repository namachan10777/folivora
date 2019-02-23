#use "./scad_ml/src/scad.ml";;

module Track = struct
    let pillar_d = 41.9


    let ball_r = 17.0
    let ball_c = 0.5

    let hole_r = 1.7
    let hole_c = 3.

    let bearing_shaft_r = 1.6
    let bearing_shaft_h = 8.

    let bearing_r = 3.5
    let bearing_c = 0.4
    let bearing_t = 4.

    let eps = 0.1

    let bearing_hollowing =
        Model.union [
            Model.cylinder bearing_shaft_r bearing_shaft_h ~fn:30
                |> Model.rotate (0., pi /. 2., 0.);
            Model.cube (bearing_shaft_r+.eps, bearing_shaft_r*.2.,  bearing_shaft_h)
                |> Model.rotate (0., pi /. 2., 0.)
                |> Model.translate (0., -.bearing_shaft_r, bearing_shaft_r +. eps);
            Model.cylinder (bearing_r+.bearing_c) bearing_t ~fn:30
                |> Model.rotate (0., pi /. 2., 0.)
                |> Model.translate ((bearing_shaft_h -. bearing_t) /. 2., 0., 0.)
        ]

    let bearinghedge r =
        let hole = bearing_hollowing
            |> Model.translate (-.bearing_shaft_h/.2., r, 0.) in
        Model.union [
            hole;
            hole |> Model.rotate (0., 0.,    pi *. 2. /. 3.);
            hole |> Model.rotate (0., 0., -. pi *. 2. /. 3.)
        ]

    let foundation_bottom = (pillar_d +. hole_c *. 2., pillar_d)
    let foundation_center = (pillar_d /. 2. +. hole_c, pillar_d /. 2. +. hole_c)
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
        let base = Model.cube (fst foundation_bottom, snd foundation_bottom, 100.) in
        let cutter = Model.cube (200., 200., 200.) |> Model.translate (-.100., -.100., 0.) in
        Model.difference base [
            cutter
            |> Model.rotate (0., tilt, 0.)
            |> Model.translate (top_surface_center tilt offset)]

    let screw_holes =
        let points = [
            (hole_c, snd foundation_center, 0.);
            ((fst foundation_bottom) -. hole_c, snd foundation_center, 0.);
        ] in
        let screw_holes = Model.cylinder ~fn:30 hole_r 50. in
        Model.union @@ List.map (fun p -> Model.translate p screw_holes) points

    let foundation tilt offset =
        let bearinghedge_r = sqrt ((ball_r +. bearing_r) ** 2. -. offset**2.) in
        let bearinghedge_h = ball_r -. offset *. (cos tilt) in
        Model.difference (foundation_body tilt offset) [
            bearinghedge bearinghedge_r
            |> Model.rotate (0., 0., pi)
            |> Model.rotate (0., tilt, 0.)
            |> Model.translate (bearinghedge_center tilt offset);
            Model.sphere (ball_r +. ball_c) ~fn:50
            |> Model.translate (fst foundation_center, snd foundation_center, ball_r);
            screw_holes
        ]
end