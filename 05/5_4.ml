(* 5.4 *)
(* 目的: 2次方程式の判別式の値を算出する *)
(* seiza : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c
                                     
let t1 = hanbetsushiki 1. 2. 3. = -8.
let t2 = hanbetsushiki 2. 3. 1. = 1.
let t3 = hanbetsushiki 3. 2. 1. = -8.
       
(* 5.5 *)  
(* 目的: 2次方程式の解の個数を算出する *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c = 
  if hanbetsushiki a b c == 0. then 1
  else if hanbetsushiki a b c > 0. then 2
  else 0
  
let t4 = kai_no_kosuu 1. 2. 3. = 0
let t5 = kai_no_kosuu 2. 3. 1. = 2
let t6 = kai_no_kosuu 1. 2. 1. = 1
         
(* 5.6 *)
(* 目的: 2次方程式が虚数解を持つかを判定する *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosuukai a b c = hanbetsushiki a b c < 0.
                      
let t7 = kyosuukai 1. 2. 3. = true
let t8 = kyosuukai 2. 3. 1. = false