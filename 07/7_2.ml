(* 成績から評価を判定する *)
(* judge : float -> string *)
let judge score = if score = 100. then "A" else "F"

(* FIXME: seiseki_msg とかでは? *)
(* IMO: メッセージのformatting(seiseki) と 成績自体の判定(judge) は別にしたいので分けた *)
(* 氏名と成績から評価メッセージを作成する *)
(* seiseki : string * float -> string *)
let seiseki name_score = match name_score with
    (name, score) -> name ^ " さんの評価は " ^ judge score ^ " です。"

(* tests *)
let t1 = seiseki ("閣晋", 99.) = "閣晋 さんの評価は F です。"
let t1 = seiseki ("閣晋", 100.) = "閣晋 さんの評価は A です。"