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
            Pad.near = [];
            Pad.far = [];
            Pad.diff = (0.0, 0.0, 0.0);
            Pad.center = (16.0, cherry_mx)
        };
        {
            Pad.w = 16.0;
            Pad.near = [];
            Pad.far = [];
            Pad.diff = (2.0, 2.0, -1.0);
            Pad.center = (16.0, cherry_mx)
        };
        {
            Pad.w = 16.0;
            Pad.near = [];
            Pad.far = [];
            Pad.diff = (2.0, 2.0, -2.0);
            Pad.center = (16.0, cherry_mx)
        };
        {
            Pad.w = 16.0;
            Pad.near = [];
            Pad.far = [];
            Pad.diff = (2.0, -2.0, 2.0);
            Pad.center = (16.0, cherry_mx)
        };
    ];
}

let () =
    build (Pad.f pad_conf) "pad.scad"
