(* 目的: f g を受け取り f(g(x)) となる合成関数を返す *)
(* compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose f g = let c x = f (g x) in c

(* examples *)
let add3 x = x + 3
let time2 x = x * 2

(* tests *)
let t1 = (compose time2 add3) 4 = 14
let t2 = (compose add3 time2) 4 = 11
