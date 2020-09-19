let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Key_unit.cherry_mx
let pi = Scad_ml.Core.pi

let () =
	let (model, _) = Thumb.f [
		(key_size, cherry_mx, pi /. 12.);
		(key_size, cherry_mx, pi /. 6.);
		(key_size, cherry_mx, pi /. 12.);
	]
	((19.0, 19.0, 5.0), Key_unit.cherry_mx)
	[
		(key_size, cherry_mx, pi /. 12.);
		(key_size, cherry_mx, pi /. 6.);
		(key_size, cherry_mx, pi /. 12.);
	]
	in
	let pad = Pad.f [
		{
			Pad.d = (0.0, 0.0, 0.0);
			Pad.base = (key_size, cherry_mx);
			Pad.far = [
				(key_size, cherry_mx, pi /. 6.);
			];
			Pad.near = [
				(key_size, cherry_mx, pi /. 12.);
			];
		};
		{
			Pad.d = (1.0, 0.0, 0.0);
			Pad.base = (key_size, cherry_mx);
			Pad.far = [
				(key_size, cherry_mx, pi /. 6.);
				(key_size, cherry_mx, pi /. 6.);
			];
			Pad.near = [
				(key_size, cherry_mx, pi /. 12.);
			];
		};
		{
			Pad.d = (1.0, 0.0, 0.0);
			Pad.base = (key_size, cherry_mx);
			Pad.far = [
				(key_size, cherry_mx, pi /. 6.);
				(key_size, cherry_mx, pi /. 6.);
			];
			Pad.near = [
				(key_size, cherry_mx, pi /. 12.);
			];
		};
	]
	in
	build pad "pad.scad";
	build model "thumb.scad";
