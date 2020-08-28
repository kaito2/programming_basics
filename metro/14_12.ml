#use "eki.ml"
#use "12.ml"

(* 
 MEMO: 
 問題の 
 > ここで作った名前のない関数は、問題14.11で作った2つの名前のない関数とどのような関係にあるか
 は、compose の関係にあると思われるが、無名関数で compose を作ると汚くなるのでやめた。
 *)

(* 目的: station_name_t のリストと始発の漢字駅名を受け取り、初期化された station_node_t のリストを返す *)
(* make_initial_station_node_list : string -> station_name_t list -> station_node_t list *)
let make_initial_station_node_list start_station_kanji ekimei_lst = 
    List.map
        (fun ekimei -> if ekimei.kanji = start_station_kanji
            then {name=ekimei.kanji; shortest_distance_km=0.; path=[ekimei.kanji]}
            else {name=ekimei.kanji; shortest_distance_km=infinity; path=[]})
        ekimei_lst

(* tests *)
let given_station_name_list = [
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
]
let expected_station_node_list = [
    {name="代々木上原"; shortest_distance_km=infinity; path=[]};
    {name="代々木公園"; shortest_distance_km=0.; path=["代々木公園"]};
    {name="明治神宮前"; shortest_distance_km=infinity; path=[]};
]
let t1 = make_initial_station_node_list "代々木公園" given_station_name_list = expected_station_node_list
