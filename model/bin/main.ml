let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Key_unit.cherry_mx
let pi = Scad_ml.Core.pi

let pad_conf = {
    Pad.key_t = 5.0;
    cols = [
        {
            Pad.w = 16.0;
            Pad.pos = (0.0, 0.0, 0.0);
            Pad.angle = (-.pi/.3.0, pi/.20., pi/.20.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (17.0, 0.0, -2.5);
            Pad.angle = (-.pi/.3.0, 0., pi/.40.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (34.0, 3.0, -4.5);
            Pad.angle = (-.pi/.3.0, 0., 0.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (51.0, 2.0, -3.5);
            Pad.angle = (-.pi/.3.0, 0., -.pi/.40.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (68.0, -1.0, 1.0);
            Pad.angle = (-.pi/.3.0, 0., -.pi/.20.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (85.0, -4.0, 1.0);
            Pad.angle = (-.pi/.3.0, -.pi/.20., -.pi/.15.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
    ];
}

let thumb_conf = {
    Pad.key_t = 6.0;
    cols = [
        {
            Pad.w = 16.0;
            Pad.pos = (-15.0, 0., 10.0);
            Pad.angle = (0., 1.*.pi/.5., 0.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, 0., cherry_mx);
                (16.0, pi/.6., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (0.0, 0.0, 0.0);
            Pad.angle = (0., 0., 0.);
            Pad.offset = 0;
            Pad.keys = [
                (16.0, 0., cherry_mx);
            ];
        };
        {
            Pad.w = 16.0;
            Pad.pos = (27.0, -18.0, 2.0);
            Pad.angle = (pi/.6., -2.*.pi/.5., 0.);
            Pad.offset = 1;
            Pad.keys = [
                (16.0, 0., cherry_mx);
                (16.0, -.pi/.6., cherry_mx);
            ];
        };
    ];
}

let conf = {
    Pad.main_pad = pad_conf;
    Pad.pad_tilt = (0., pi/.20., 0.);
    Pad.thumb_pad = thumb_conf;
    Pad.thumb_tilt = (0., 0., 0.);
    Pad.thumb_pos = (-10., -20., -20.);
    Pad.wrist_switch = cherry_mx;
    Pad.wrist_switch_size = (60., 40., 10.);
    Pad.wrist_switch_offset = (30., 20.);
    Pad.wrist_switch_pos = (30., -90., -30.);
}

let () =
    build (Pad.f conf) "pad.scad"
