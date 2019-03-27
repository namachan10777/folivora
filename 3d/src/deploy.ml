#use "./scad_ml/src/scad.ml"
#use "./src/track.ml"
#use "./src/block.ml"
#use "./src/key.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

let () =
    build (Block.anchorage 3.0 (angle_unit *. 1.)) "block0.scad";
    build (Block.anchorage 3.0 (angle_unit *. 0.)) "block1.scad";
    build (Block.anchorage 4.0 (angle_unit *. 2.)) "block2.scad";
    build (Track.foundation 4.0) "foundation-left.scad";
    build (Track.foundation 4.0 |> Model.mirror (1, 0, 0)) "foundation-right.scad";
    build (Track.cover 4.0 10.0) "cover-left.scad";
    build (Track.cover 4.0 10.0 |> Model.mirror (1, 0, 0)) "cover-right.scad";
    build (Key.key_module [
        (1, 2, (2., 0., 0.));
        (1, 2, (2., 2., -.1.));
        (1, 2, (2., 4., -.4.));
        (1, 2, (2., 3., -.2.));
        (1, 1, (2., 0., 0.));
        (1, 1, (2., 0., 0.));
    ]) "key.scad"
