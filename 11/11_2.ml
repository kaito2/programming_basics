(* 目的: 以下の漸化式で定義されるような数列の a(n) を求める
    a(0) = 3.
    a(n) = 2 * a(n-1) - 1. (n >= 1)
*)
(* a : int -> int *)
let rec a n = 
    if n = 0
        then 3
        else 2 * a (n - 1) - 1

(* tests *)
let t1 = a 0 = 3
let t2 = a 1 = 5
let t3 = a 2 = 9
