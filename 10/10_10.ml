#use "../common/station.ml"

(* 目的: ローマ字の駅名と駅名のリストを受け取り、漢字表記を返す *)
(* romaji_to_kanji : string -> station_name_t -> "string" *)
let rec romaji_to_kanji romaji station_name_list = match station_name_list with
      [] -> ""
    | {kanji=k; romaji=r} :: rest ->
        if romaji = r
            then k
            else romaji_to_kanji romaji rest

(* tests *)
let t1 = romaji_to_kanji "myogadani" global_station_name_list = "茗荷谷"
let t2 = romaji_to_kanji "yoyogikouen" global_station_name_list = "代々木公園"
let t3 = romaji_to_kanji "otemachi" global_station_name_list = "大手町"
let t4 = romaji_to_kanji "NON-EXISTENT-STATION-NAME" global_station_name_list = ""
