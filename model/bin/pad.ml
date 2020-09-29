module M = Scad_ml.Model
open Scad_ml.Util

type col_t = {
    w : float;
    diff : Scad_ml.Math.t;
    near : (float * float * (Scad_ml.Math.t -> Scad_ml.Core.scad_t)) list;
    far : (float * float * (Scad_ml.Math.t -> Scad_ml.Core.scad_t)) list;
    center : float * (Scad_ml.Math.t -> Scad_ml.Core.scad_t);
}

type pad_conf_t = {
    key_t : float;
    cols : col_t list;
}

let gen_base_with_joint t =
    let rec f p_acc = function
    | [] -> []
    | (w, d, diff, gen) :: [] -> [gen (w, d, t) |>> (diff <+> p_acc)]
    | (w, d, diff, gen) :: ((_, d', diff', _) :: _ as remain) ->
        (gen (w, d, t) |>> (diff <+> p_acc))
        :: (M.hull [
            M.cube (0.001, d, t);
            M.cube (0.001, d', t) |>> diff';
        ] |>> (p_acc <+> (w, 0., 0.) <+> diff))
        :: f (p_acc <+> diff <+> (w, 0., 0.)) remain
    in
    f (0., 0., 0.)

let f conf =
    conf.cols
    |> List.map (fun col -> (col.w, fst col.center, col.diff, snd col.center))
    |> gen_base_with_joint conf.key_t
    |> M.union
