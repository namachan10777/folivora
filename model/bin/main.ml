let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let () =
	let (model, _) = Thumb.f [
		((19.0, 19.0, 5.0), Key_unit.cherry_mx, Scad_ml.Core.pi /. 12.);
		((19.0, 19.0, 5.0), Key_unit.cherry_mx, Scad_ml.Core.pi /. 6.);
		((19.0, 19.0, 5.0), Key_unit.cherry_mx, Scad_ml.Core.pi /. 12.);
	]
	((19.0, 19.0, 5.0), Key_unit.cherry_mx)
	[
		((19.0, 19.0, 5.0), Key_unit.cherry_mx, Scad_ml.Core.pi /. 12.);
		((19.0, 19.0, 5.0), Key_unit.cherry_mx, Scad_ml.Core.pi /. 6.);
		((19.0, 19.0, 5.0), Key_unit.cherry_mx, Scad_ml.Core.pi /. 12.);
	]
	in
	build model "thumb.scad"
