type thumb_config = {
	key_size: float * float * float;
	key: (float * float * float) -> Scad_ml.Core.scad_t;
	rotate: float;
}

module M = Scad_ml.Model
open Scad_ml.Util

let rec left (x, y, z) theta phantom = function
	| [] -> []
	| ((w, d, h), gen, rot) :: remain ->
		let phantom' = M.cube (0.01, d, h) in
		let padding = M.hull [
			phantom;
			phantom' |@> (0., 0., theta +. rot) |>> (x, y, z);
		] in
		let instance = gen (w, d, h) |>> (-.w, 0., 0.) |@> (0., 0., rot +. theta) |>> (x, y, z)
		in
		padding :: instance ::
			left
				(x -. w *. cos (rot +. theta), y -. w *. sin (rot +. theta), 0.)
				(theta +. rot)
				(phantom' |>> (-.w, 0., 0.) |@> (0., 0., rot +. theta) |>> (x, y, z))
				remain

let rec right (x, y, z) theta phantom = function
	| [] -> []
	| ((w, d, h), gen, rot) :: remain ->
		let phantom' = M.cube (0.01, d, h) in
		let padding = M.hull [
			phantom;
			phantom' |@> (0., 0., theta -. rot) |>> (x, y, z);
		] in
		let instance = gen (w, d, h) |@> (0., 0., theta -. rot) |>> (x, y, z)
		in
		padding :: instance ::
			right
				(x +. w *. cos (theta -. rot), y +. w *. sin (theta -. rot), 0.)
				(theta -. rot)
				(phantom' |>> (w, 0., 0.) |@> (0., 0., theta -. rot) |>> (x, y, z))
				remain

let f l c r =
	let (cw, cd, ch) = fst c in
	let l = left (0., 0., 0.) 0. (M.cube (0.01, cd, ch)) l in
	let r = right (cw, 0., 0.) 0. (M.cube (0.01, cd, ch) |>> (cw, 0., 0.)) r in
	let body = M.union (((snd c) (fst c)) :: l @ r) in
	(body, 0)
