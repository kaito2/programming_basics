(* 目的: 誕生日(月と日)から星座を解決する *)
(* seiza : int -> int -> string *)
(* FIXME: switch... *)
let seiza month day = 
  if month * 100 + day <= 119 then "山羊座"
  else if month * 100 + day <= 218 then "水瓶座"
  else if month * 100 + day <= 320 then "魚座"
  else if month * 100 + day <= 419 then "牡羊座"
  else if month * 100 + day <= 520 then "牡牛座"
  else if month * 100 + day <= 621 then "双子座"
  else if month * 100 + day <= 722 then "蟹座"
  else if month * 100 + day <= 822 then "獅子座"
  else if month * 100 + day <= 922 then "乙女座"
  else if month * 100 + day <= 1023 then "天秤座"
  else if month * 100 + day <= 1122 then "蠍座"
  else if month * 100 + day <= 1221 then "射手座"
  else "山羊座" 
    
let t1 = seiza 10 17 = "天秤座"
let t2 = seiza 12 31 = "山羊座"
let t3 = seiza 8  11 = "獅子座"