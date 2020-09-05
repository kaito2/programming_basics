#use "station.ml"
#use "12.ml"
#use "15_4.ml"
#use "13_6.ml"
#use "10_10.ml"
#use "14_12.ml"

(* 
 目的: station_node_t の未確定のリストと station_edge_t の駅間のリストを受け取り、
 各駅についての最短距離と最短経路のリストを返す
 *)
(* dijkstra_main: station_t list -> station_edge_list -> stataion_t list *)
let rec dijkstra_main stations station_edges = 
    if List.length stations = 0
        then []
        else 
            let (shortest_station, rest_stations) = separate_min_shortest_station stations in
            let updated_stations = update station_edges shortest_station rest_stations in
            shortest_station :: dijkstra_main updated_stations station_edges


(* examples *) 
let eki1 = {name="池袋"; shortest_distance_km = infinity; path = []} 
let eki2 = {name="新大塚"; shortest_distance_km = 1.2; path = ["新大塚"; "茗荷谷"]} 
let eki3 = {name="茗荷谷"; shortest_distance_km = 0.; path = ["茗荷谷"]} 
let eki4 = {name="後楽園"; shortest_distance_km = infinity; path = []} 

let lst = [eki1; eki2; eki3; eki4] 
 
(* tests *)
let test1 = dijkstra_main [] global_station_edge_list = [] 
let test2 = dijkstra_main lst global_station_edge_list = 
  [{name = "茗荷谷"; shortest_distance_km = 0.; path = ["茗荷谷"]}; 
   {name = "新大塚"; shortest_distance_km = 1.2; path = ["新大塚"; "茗荷谷"]}; 
   {name = "後楽園"; shortest_distance_km = 1.8; path = ["後楽園"; "茗荷谷"]}; 
   {name = "池袋"; shortest_distance_km = 3.; path = ["池袋"; "新大塚"; "茗荷谷"]}]


(* 16.5 *)
(* 目的: 始点のローマ字駅名と終点のローマ字駅名を受け取り、最短経路を返す *)
let dijkstra from_romaji_name to_romaji_name = 
    let unique_global_station_name_list = sort_and_remove_duplicated global_station_name_list in
    let from_kanji_name = romaji_to_kanji from_romaji_name unique_global_station_name_list in
    let to_kanji_name = romaji_to_kanji to_romaji_name unique_global_station_name_list in
    let station_node_list = make_initial_station_node_list from_kanji_name unique_global_station_name_list in
    let shortest_station_nodes = dijkstra_main station_node_list global_station_edge_list in
    match List.filter (fun sn -> sn.name = to_kanji_name) shortest_station_nodes with
          (* TODO: error handling... *)
          [] -> {name="undefined"; shortest_distance_km=infinity; path=[]}
        | first :: rest -> first


(* tests *) 
let test1 = dijkstra "shibuya" "gokokuji" 
    = {
        name = "護国寺"; 
        shortest_distance_km = 9.8; 
        path = 
            ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; "青山一丁目"; "表参道"; "渋谷"]
        } 
let test2 = dijkstra "myogadani" "meguro" = 
  {name = "目黒"; shortest_distance_km = 12.7000000000000028; 
   path = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]} 
 
(* 最短距離が 12.7 にならないのは、小数を２進数で表現するときの誤差のため。 
   ここではテスト結果も書いたが、これをテスト作成時に予想するのは無理なので 
   テストとして書く意味はあまりない。*) 