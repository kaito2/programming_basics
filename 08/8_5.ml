(* 8.5 *)
(* 駅名を表現する構造体 *)
type station_name_t = {
    kanji : string;
    kana : string;
    romaji : string;
    shozoku : string;
}

(* 8.6 *)
(* station_name_t 構造体をフォーマットした文字列を返す *)
(* hyoji : station_name_t -> string *)
let hyoji ekimei = match ekimei with
    {shozoku = s; kanji = kanji; kana = kana} ->
        s ^ ", " ^ kanji ^ " (" ^ kana ^ ")"

(* tests *)
let t1 = hyoji {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"}
    = "丸ノ内線, 茗荷谷 (みょうがだに)"
(* テストケース省略 *)

(* 8.7 *)
type ekikan_t = {
    kiten : string;
    shuten : string;
    keiyu : string;
    kyori_km : float;
    jikan_minutes : int;
}

let tokyo_to_myogadani = {
    kiten="東京";
    shuten="茗荷谷";
    keiyu="しらん";
    kyori_km=10.;
    jikan_minutes=5;
}