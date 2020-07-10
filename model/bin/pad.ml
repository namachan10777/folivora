module M = Scad_ml.Model

type size_t = float * float * float
type column_t = (size_t * (size_t -> Scad_ml.Core.scad_t) * float) list

type col = {
	d: size_t;
	base: size_t * (size_t -> Scad_ml.Core.scad_t);
	far: column_t;
	near: column_t;
}

let f _ = M.union []
