#use "station.ml"
#use "12.ml"
#use "../10/10_11.ml"

(* ヘルパー関数 *)
let get_global_station_distance = get_station_distance global_station_edge_list

(* 
 目的: from_station と to_station がつながっているかを判定し、
      必要に応じて to_station を更新し、返す。
 *)
(* update : station_node_t -> station_node_t -> station_node_t *)
let update from_station to_station = 
    let station_distance = get_global_station_distance from_station.name to_station.name in
    let new_distance = from_station.shortest_distance_km +. station_distance in
        if station_distance != infinity && new_distance < to_station.shortest_distance_km
            then {name=to_station.name; shortest_distance_km=new_distance; path=to_station.name :: from_station.path}
            else to_station

(* tests (コピペ) *)
let small_station_edge_list = [ 
    {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2}; 
    {kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
]
let yoyogiuehara = {name="代々木上原"; shortest_distance_km=0.; path=["代々木上原"]}
let yoyogikouen = {name="代々木公園"; shortest_distance_km=infinity; path=[]}
let meijijinguumae = {name="明治神宮前"; shortest_distance_km=infinity; path=[]}
let yoyogikouen_updated = {name="代々木公園"; shortest_distance_km=1.0; path=["代々木公園"; "代々木上原"]}

(* updated *)
let t1 = update yoyogiuehara yoyogikouen = yoyogikouen_updated
(* not updated *)
let t2 = update yoyogiuehara meijijinguumae = meijijinguumae
