(* 目的: 受け取った lst をクイックソートで昇順に整列する *)
(* quick_srot : int list -> int lst *)
let rec quick_srot lst = 
    (* 目的: lst の中から n より p である要素のみを取り出す *)
    (* take : int list -> int list *)
    let take n lst p = List.filter (fun item -> p item n) lst in
    (* 目的: lst の中から n より大きいものと n より小さいものを取り出す *)
    (* take_less (take_greater) : int list -> int list *)
    (* 15.1 *)
    let take_less_or_equal n lst = take n lst (<=) in
    let take_greater n lst = take n lst (>) in
    match lst with
          [] -> []
        | first :: rest -> 
            quick_srot (take_less_or_equal first rest)
                @ [first]
                @ quick_srot (take_greater first rest)

(* tests *)
let t1 = quick_srot [] = []
let t2 = quick_srot [1] = [1]
let t3 = quick_srot [1; 2] = [1; 2]
let t4 = quick_srot [2; 1] = [1; 2]
let t5 = quick_srot [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]

(* 正しい挙動をしない *)
(* 
補足: 
同じ値の重複が考慮されていない
したがって、 take_greater または take_less に値が等しい場合を追加する
 *)
let t6 = quick_srot [5; 5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 5; 8; 9]