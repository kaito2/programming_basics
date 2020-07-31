(* 目的: 文字列のリストから前から順に連結した文字列を返す *)
(* concat : string list -> string *)
let rec concat lst = match lst with
      [] -> ""
    | first :: rest -> first ^ concat rest

(* tests *)
let t1 = concat [] = ""
let t2 = concat ["春"] = "春"
let t3 = concat ["春"; "夏"; "秋"; "冬";] = "春夏秋冬"
