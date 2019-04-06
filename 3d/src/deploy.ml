#use "./src/common.ml"
#use "./src/key.ml"
#use "./src/track.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

let () =
    build (Track.nut_insert) "nutinsert.scad";
    build (Track.thumb_keys) "thumbkey.scad";
    build (Track.thumb_track) "thumbtrack.scad";
    build (Track.trackball_cover) "trackball_cover.scad";
    build (Key.key_module [
        (1, 2, (2.54, 0., 0.));
        (1, 2, (2.54, 2., -.1.));
        (1, 2, (2.54, 5., -.2.));
        (1, 2, (2.54, 3., -.1.));
        (1, 1, (2.54, 0., -.1.));
        (1, 1, (2.54, 0., -.1.));
    ]) "key.scad"
