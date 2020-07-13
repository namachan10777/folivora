module M = Scad_ml.Model
open Scad_ml.Util

type size_t = float * float * float
type column_t = (size_t * (size_t -> Scad_ml.Core.scad_t) * float) list

type col = {
	d: size_t;
	base: size_t * (size_t -> Scad_ml.Core.scad_t);
	far: column_t;
	near: column_t;
}

let gen_col c =
	let center_size = fst c.base in
	let (_, d, h) = center_size in
	let center = (snd c.base) center_size |>> (0., 0., -.h) in
	let rec gen trans p angle_acc = function
		| [] -> []
		| key :: remain ->
			let (key_size, key_gen, angle) = key in
			let (w, d, h) = key_size in
			let (x, y, z) = p in
			let angle_acc' = angle_acc +. angle in
			let plate = M.cube (w, 0.0001, h) |>> (0., 0., -.h) in
			let joint = M.hull [
				plate |@> (angle_acc, 0.0, 0.0) |>> p;
				plate |@> (angle_acc+.angle, 0.0, 0.0) |>> p;
			] in
			let key = trans key_gen key_size  |>> (0., 0., -.h) |@> (angle_acc', 0.0, 0.0) |>> p in
			key :: joint :: gen trans (x, y +. d *. cos angle_acc', z +. d *. sin angle_acc') angle_acc' remain
	in
	let far = gen (fun gen size -> gen size) (0.0, d, 0.0) 0.0 c.far in
	let near =
		gen (fun gen size -> gen size |> M.mirror (0, 1, 0) |>> (0., get_y size, 0.)) (0.0, 0.0, 0.0) 0.0 c.near
		|> M.union |> M.mirror (0, 1, 0)
	in
	M.union (center :: near :: far)

let f = function
	| [] -> M.union []
	| c :: []  -> gen_col c
	| c1 :: _ :: _ -> gen_col c1
