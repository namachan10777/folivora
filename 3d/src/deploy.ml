#use "./src/common.ml"
#use "./src/key.ml"
#use "./src/thumb.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

let () =
    build (Thumb.nut_insert) "nutinsert.scad";
    build (Thumb.thumb_keys) "thumbkey.scad";
    build (Thumb.thumb_track) "thumbtrack.scad";
    build (Thumb.trackball_cover) "trackball_cover.scad";
    build (Key.key_main [
        (2, 0., 0.);
        (2, 2., -.1.);
        (2, 5., -.2.);
        (2, 3., -.1.);
        (1, -3., -.1.);
        (1, -5., -.2.);
    ]) "key.scad"
