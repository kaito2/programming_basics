(* 目的: 時間が午前か午後かを判定する *)
(* jikan : int -> string *)
(* FIXME: validate argument *)
let jikan hour = 
  if hour >= 12 then "午後"
                else "午前"
    
let t1 = jikan 1  = "午前"
let t2 = jikan 18 = "午後"
let t3 = jikan 12 = "午後"