(* 目的: n までの自然数の降順のリストを返す *)
(* enumerate : int -> int list *)
let rec enumerate n = 
    if n <= 0
        then []
        else n :: enumerate (n - 1)

(* tests *)
let t1 = enumerate 0 = []
let t2 = enumerate 3 = [3; 2; 1]
let t3 = enumerate (- 10) = []

(* 14.15 *)
(* 目的: n までの自然数の和を返す *)
(* one_to_n : int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0

(* tests *)
let t2_1 = one_to_n 0 = 0
let t2_2 = one_to_n 10 = 55
let t2_3 = one_to_n 12 = 78

(* 14.16 *)
(* 目的: n の階乗を返す *)
(* fac : int -> int *)
let fac n = List.fold_right ( * ) (enumerate n) 1

(* tests *)
let t3_1 = fac 1 = 1
let t3_2 = fac 10 = 3628800
