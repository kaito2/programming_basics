(* 目的: 0から受け取った数までの2乗の和を求める *)
(* sum_of_square : float -> float *)
(* FIXME: 0 より小さい数の vlidation *)
(* FIXME: int で十分なので累乗演算子を自作する? *)
let rec sum_of_square num = 
    if num = 0.
        then 0.
        else num ** 2. +. sum_of_square (num -. 1.)

(* tests *)
let t1 = sum_of_square 1. = 1.
let t2 = sum_of_square 2. = 5.
let t3 = sum_of_square 3. = 14.
let t4 = sum_of_square 0. = 0.
