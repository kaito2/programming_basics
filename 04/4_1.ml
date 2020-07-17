(* 4.1 *)
(* baito_kyuyo : int -> int -> int *)
let baito_kyuyo years hours = (850 + 100 * years) * hours 
let kyuyo = baito_kyuyo 3 5 
    
(* 4.2 *)
(* jikoshokai : string -> string *)
let jikoshokai name = "私の名前は" ^ name ^ "です。よろ。"
let msg = jikoshokai "閣晋"
    
(* 4.3 *)
(* hyojun_taiju : float -> float *)
let hyojun_taiju height_meter = height_meter ** 2. *. 22.
let my_hyojun_taiju = hyojun_taiju 1.76
    
(* 4.4 *)
(* bmi : float -> float -> float *)
let bmi height_meter weight_kg = weight_kg /. (height_meter ** 2.)
let my_bmi = bmi 1.76 60.0