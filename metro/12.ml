(* #use "eki.ml" *)

(* 注釈: 書籍で定義されている型を以下のようにマッピングしました *)
(* eki_t => station_node_t *)
(* seiretsu => sort_and_remove_duplicated *)

(* 12.1 *)
(* ある駅からの経路と最短距離を表す *)
(* MEMO: '駅'という概念を表す構造体ではないのでもう少し適切な名前がありそう *)
(* MEMO: 始発駅を格納したほうが良いのでは? -> path の末尾を見ればよいのか *)
type station_node_t = {
    name : string;
    shortest_distance_km : float;
    (* 始発駅が末尾になるように name で示されるまでの漢字駅名が格納される *)
    path : string list;
}


(* 12.2 *)
(* 目的: station_name_t方のリストを受け取り、その駅名を使って station_node_t のリストを返す *)
(* make_station_list : station_name_t list -> station_node_t list *)
let rec make_station_list station_name_list = 
    List.map (fun s -> {name=s.kanji; shortest_distance_km=infinity; path=[]}) station_name_list

(* examples *)
let small_station_name_list = [
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
]

(* tests *)
let t1 = make_station_list [] = []
let expected_station_list = [
    {name="代々木上原"; shortest_distance_km=infinity; path=[]};
    {name="代々木公園"; shortest_distance_km=infinity; path=[]};
    {name="明治神宮前"; shortest_distance_km=infinity; path=[]};
]
let t2 = make_station_list small_station_name_list = expected_station_list


(* 12.3 *)
(* 目的: station_node_t のリストと起点駅の漢字名を受け取り、初期化されたリストを返す *)
(* init_station_list : station_node_t list -> string -> staton_t list *)
let rec init_station_list station_list start_station_kanji =
    List.map
        (fun s -> if s.name = start_station_kanji
            then {name=s.name; shortest_distance_km=0.; path=[s.name]}
            else s)
        station_list

(* examples *)
let small_station_list = [
    {name="代々木上原"; shortest_distance_km=infinity; path=[]};
    {name="代々木公園"; shortest_distance_km=infinity; path=[]};
    {name="明治神宮前"; shortest_distance_km=infinity; path=[]};
]

(* tests *)
let t1 = init_station_list [] "代々木公園" = []
let expected_station_list_2 = [
    {name="代々木上原"; shortest_distance_km=infinity; path=[]};
    {name="代々木公園"; shortest_distance_km=0.; path=["代々木公園"]};
    {name="明治神宮前"; shortest_distance_km=infinity; path=[]};
]
let t2 = init_station_list small_station_list "代々木公園" = expected_station_list_2


(* 12.4 *)
(* ソートと重複削除を別々で作成する *)

(* 12.4-a *)
(* 目的: ひらがな駅名の辞書順でソートされている station_name_t のリストと station_name_t を受け取り、辞書順で適当な位置に挿入する *)
(* insert_ekimei : station_name_t list -> station_name_t -> station_name_t list *)
let rec insert_ekimei station_name_list target_ekimei = match station_name_list with
      [] -> [target_ekimei]
    | ({kana=k} as first) :: rest ->
        let {kana=target_kana} = target_ekimei in
        if k > target_kana
            then target_ekimei :: station_name_list
            else first :: insert_ekimei rest target_ekimei

(* tests *)
let a = {kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="丸ノ内線"}
let i = {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}
let u = {kanji="浦安"; kana="うらやす"; romaji="urayasu"; shozoku="東西線"}
let e = {kanji="江戸川橋"; kana="えどがわばし"; romaji="edogawabasi"; shozoku="有楽町線"}
let o = {kanji="王子"; kana="おうじ"; romaji="oji"; shozoku="南北線"}

let t1 = insert_ekimei [] u = [u]
let t1 = insert_ekimei [a; i; e; o] u = [a; i; u; e; o]

(* 12.4-b *)
(* 目的: station_name_t のリストを受け取り、ひらがな駅名でのソート結果を返す *)
(* sort_station_name_list : station_name_t list -> station_name_t list *)
let rec sort_station_name_list station_name_list = match station_name_list with
      [] -> []
    | first :: rest -> insert_ekimei (sort_station_name_list rest) first

(* tests *)
let t1 = sort_station_name_list [] = []
let t2 = sort_station_name_list [a] = [a]
let t3 = sort_station_name_list [a; u; o; e; i] = [a; i; u; e; o]


(* 12.4-c *)
(* 目的: ひらがな駅名の辞書順でソートされた station_name_t のリストを受け取り、ひらがな駅名の重複を削除する *)
(* NOTE: どの路線の station_name_t が残るかは非決定的 *)

(* remove_duplicated_ekimei_rec : station_name_t list -> string -> station_name_t list *)
let rec remove_duplicated_ekimei_rec station_name_list prev_kana = match station_name_list with
      [] -> []
    | ({kana=k} as first) :: rest -> 
        if k = prev_kana
            then remove_duplicated_ekimei_rec rest prev_kana
            else first :: remove_duplicated_ekimei_rec rest k

(* remove_duplicated_ekimei : station_name_t list -> station_name_t list *)
let remove_duplicated_ekimei station_name_list = match station_name_list with
      [] -> []
    | ({kana=k} as first) :: rest -> first :: remove_duplicated_ekimei_rec rest k

(* tests *)
let i2 = {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}
let i3 = {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="テスト用線"}

let t1 = remove_duplicated_ekimei [] = []
let t2 = remove_duplicated_ekimei [a; i; u] = [a; i; u]
let t3 = remove_duplicated_ekimei [a; i; i2; i3; u] = [a; i; u]


(* 本体 *)
(* 目的: station_name_t のリストを受け取り、ひらがな駅名での重複削除とソートが行われたリストを返す *)
(* sort_and_remove_duplicated : station_name_t list -> station_name_t list *)
let sort_and_remove_duplicated station_name_list = remove_duplicated_ekimei (sort_station_name_list station_name_list)

(* tests *)
let actual = sort_and_remove_duplicated [a; i; u; i2; o; e; i3]
(* NOTE: 重複するひらがな駅名がある場合、どの路線のものが採用されるかが不定なため、いかのいずれかにマッチすればよい。 *)
let t1 = actual = [a; i; u; e; o] || actual = [a; i2; u; e; o] || actual = [a; i3; u; e; o]
