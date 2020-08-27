(* Purpose: Find the greatest common divisor. *)
(* gcd: int -> int -> int *)
(* MEMO: with euclidean algorithm *)
let rec gcd m n = 
    if n = 0
        then m
        else gcd n (m mod n)

(* tests *)
let t1 = gcd 9 6 = 3
let t2 = gcd 13 13 = 13
let t3 = gcd 600 37 = 1
let t4 = gcd 100 20 = 20
let t5 = gcd 2061517 624129 = 18913
