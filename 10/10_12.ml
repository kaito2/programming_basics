#use "./10_10.ml"
#use "./10_11.ml"

let ekikan_msg s1_kanji s2_kanji distance 
    = s1_kanji ^ "駅から" ^ s2_kanji ^ "駅までは" ^ string_of_float distance ^ "kmです。"
let not_connected_msg s1_kanji s2_kanji 
    = s1_kanji ^ "駅と" ^ s2_kanji ^ "駅はつながっていません。"
let non_existent_msg s_kanji
    = s_kanji ^ "という駅は存在しません。"

(* 目的: 2駅間の距離を表すメッセージを返す *)
(* kyori_wo_hyoji : string -> string -> station_edge_t list -> string *)
let kyori_wo_hyoji s1_romaji s2_romaji station_edge_list = 
    let (s1_kanji, s2_kanji) = (romaji_to_kanji s1_romaji global_station_name_list, romaji_to_kanji s2_romaji global_station_name_list) in
        if s1_kanji = "" then non_existent_msg s1_romaji
        else if s2_kanji = "" then non_existent_msg s2_romaji
        else let distance = get_station_distance s1_kanji s2_kanji global_station_edge_list in
            if distance = infinity 
                then not_connected_msg s1_kanji s2_kanji
                else ekikan_msg s1_kanji s2_kanji distance

(* tests *)
let t1 = kyori_wo_hyoji "myogadani" "shinotsuka" global_station_edge_list 
    = "茗荷谷駅から新大塚駅までは1.2kmです。"
let t2 = kyori_wo_hyoji "shinotsuka" "myogadani" global_station_edge_list
    = "新大塚駅から茗荷谷駅までは1.2kmです。"
let t3 = kyori_wo_hyoji "myogadani" "ikebukuro" global_station_edge_list
    = "茗荷谷駅と池袋駅はつながっていません。"
let t4 = kyori_wo_hyoji "myogadani" "NON-EXISTENT-STATION-NAME" global_station_edge_list
    = "NON-EXISTENT-STATION-NAMEという駅は存在しません。"
