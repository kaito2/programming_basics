(* 目的: 文字列のリストを受け取り、連結した文字列を返す *)
(* concat : string list -> string *)
let cocat strings = List.fold_right (^) strings ""

(* tests *)
let t1 = cocat [] = ""
let t2 = cocat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
let t3 = cocat ["ヒル"; "クラ"; "イム"] = "ヒルクライム"
