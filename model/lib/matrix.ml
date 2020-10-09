type t = float array array

let rotx theta =
    [|
        [|1.0;       0.0;         0.0|];
        [|0.0; cos theta; -.sin theta|];
        [|0.0; sin theta;   cos theta|];
    |]

let roty theta =
    [|
        [|  cos theta;       0.0; sin theta|];
        [|        0.0;       1.0;       0.0|];
        [|-.sin theta;       0.0; cos theta|];
    |]

let rotz theta =
    [|
        [|cos theta; -.sin theta; 0.0|];
        [|sin theta;   cos theta; 0.0|];
        [|      0.0;         0.0; 1.0|];
    |]

let mul a b =
    let ret = Array.make_matrix 3 3 0.0 in
    for i = 0 to 2 do
        for j = 0 to 2 do
            for k = 0 to 2 do
                ret.(i).(j) <- a.(i).(k) *. b.(k).(j) +. ret.(i).(j);
            done;
        done;
    done;
    ret

let trans mat vec =
    let dot r v =
        let (x, y, z) = v in
        r.(0) *. x +. r.(1) *. y +. r.(2) *. z
    in
    (dot mat.(0) vec, dot mat.(1) vec, dot mat.(2) vec)
