type t = float array array

open Scad_ml.Util

let rotx theta =
    [| [|1.0; 0.0; 0.0|]
     ; [|0.0; cos theta; -.sin theta|]
     ; [|0.0; sin theta; cos theta|] |]

let roty theta =
    [| [|cos theta; 0.0; sin theta|]
     ; [|0.0; 1.0; 0.0|]
     ; [|-.sin theta; 0.0; cos theta|] |]

let rotz theta =
    [| [|cos theta; -.sin theta; 0.0|]
     ; [|sin theta; cos theta; 0.0|]
     ; [|0.0; 0.0; 1.0|] |]

let mul a b =
    let ret = Array.make_matrix 3 3 0.0 in
    for i = 0 to 2 do
      for j = 0 to 2 do
        for k = 0 to 2 do
          ret.(i).(j) <- (a.(i).(k) *. b.(k).(j)) +. ret.(i).(j)
        done
      done
    done ;
    ret

let rot (x, y, z) = rotx x |> mul (roty y) |> mul (rotz z)

let cross v1 v2 =
    let x1, y1, z1 = v1 in
    let x2, y2, z2 = v2 in
    ( (y1 *. z2) -. (z1 *. y2)
    , (z1 *. x2) -. (x1 *. z2)
    , (x1 *. y2) -. (y1 *. x2) )

let dot v1 v2 =
    let x1, y1, z1 = v1 in
    let x2, y2, z2 = v2 in
    (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let norm v =
    let x, y, z = v in
    sqrt ((x ** 2.) +. (y ** 2.) +. (z ** 2.))

let scale s v =
    let x, y, z = v in
    (s *. x, s *. y, s *. z)

let normalize v =
    let x, y, z = v in
    let norm = norm v in
    (x /. norm, y /. norm, z /. norm)

let n a b c = normalize @@ cross (b <-> a) (c <-> a)

let proj a b c p =
    let n = normalize @@ cross (b <-> a) (c <-> a) in
    p <-> scale (dot n (p <-> a)) n

let trans mat vec =
    let dot r v =
        let x, y, z = v in
        (r.(0) *. x) +. (r.(1) *. y) +. (r.(2) *. z)
    in
    (dot mat.(0) vec, dot mat.(1) vec, dot mat.(2) vec)
