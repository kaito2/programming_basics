(* 
    10_12.ml で 10_10.ml, 10_11.ml をインポートすると、
    ../common/eki.ml を二重でインポートしてしまいバグるためコメントアウト
*)
(* #use "../common/eki.ml" *)

(* 目的: 直接接続されている2駅の距離を取得する *)
(* get_ekikan_kyori station_edge_t list -> string -> string -> float *)
let rec get_ekikan_kyori ekikan_list s1_kanji s2_kanji = match ekikan_list with
      [] -> infinity
    | {kiten=k; shuten=s; kyori=d} :: rest -> 
        if s1_kanji = k && s2_kanji = s || s1_kanji = s && s2_kanji = k
            then d
            else get_ekikan_kyori rest s1_kanji s2_kanji

(* tests *)
(* let t1 = get_ekikan_kyori "茗荷谷" "新大塚" global_station_edge_list = 1.2 *)
(* let t2 = get_ekikan_kyori "新大塚" "茗荷谷" global_station_edge_list = 1.2 *)
(* let t3 = get_ekikan_kyori "茗荷谷" "池袋" global_station_edge_list = infinity *)
