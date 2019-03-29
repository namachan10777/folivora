#use "./src/common.ml"
#use "./src/track.ml"
#use "./src/key.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

let () =
    build (Track.foundation 4.0) "foundation-left.scad";
    build (Track.foundation 4.0 |> Model.mirror (1, 0, 0)) "foundation-right.scad";
    build (Track.cover 4.0 10.0) "cover-left.scad";
    build (Track.cover 4.0 10.0 |> Model.mirror (1, 0, 0)) "cover-right.scad";
    build (Key.key_module [
        (1, 2, (2.54, 0., 0.));
        (1, 2, (2.54, 2., -.1.));
        (1, 2, (2.54, 5., -.2.));
        (1, 2, (2.54, 3., -.1.));
        (1, 1, (2.54, 0., -.1.));
        (1, 1, (2.54, 0., -.1.));
    ]) "key.scad"
