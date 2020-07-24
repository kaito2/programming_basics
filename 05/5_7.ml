(* 4.4 から借用 *)
(* bmi : float -> float -> float *)
let bmi height_meter weight_kg = weight_kg /. (height_meter ** 2.) 

(* 5.7 *)
(* 目的: 身長と体重から体型を判定する *)
(* taikei : float -> float -> string *)
let taikei height_meter weight_kg = 
  if bmi height_meter weight_kg < 18.5 then "やせ"
  else if bmi height_meter weight_kg < 25. then "標準"
  else if bmi height_meter weight_kg < 30. then "肥満"
  else "高度肥満"
  
(* BMI: 18.4 *)
let t1 = taikei 1.76 57. = "やせ"
(* BMI: 18.5 *)
let t2 = taikei 1.76 57.31 = "標準"
(* BMI: 25. *)
let t3 = taikei 1.76 77.44 = "肥満"
(* BMI: 30. *)
let t4 = taikei 1.76 92.93 = "高度肥満"