(* まずは twice を定義 *)
let twice f = let tw x = f (f x) in tw

let double x = x * 2
let tt = twice twice
let time16 = (tt double) 1

(* output
val twice : ('a -> 'a) -> 'a -> 'a = <fun>
val double : int -> int = <fun>
val tt : ('_weak2 -> '_weak2) -> '_weak2 -> '_weak2 = <fun>
val time16 : int = 16
 *)
 