#use "eki.ml"
#use "12.ml"
#use "10_11.ml"

(* tests *)
let yoyogiuehara = {name="代々木上原"; shortest_distance_km=0.; path=["代々木上原"]}
let yoyogikouen = {name="代々木公園"; shortest_distance_km=infinity; path=[]}
let meijijinguumae = {name="明治神宮前"; shortest_distance_km=infinity; path=[]}
let yoyogikouen_updated = {name="代々木公園"; shortest_distance_km=1.0; path=["代々木公園"; "代々木上原"]}

(* 13.7 *)
(* 目的: 基点となる駅と駅のリストを受け取り、更新されたリストを返す *)
let update from_station lst = 
    let get_global_ekikan_kyori = get_ekikan_kyori global_station_edge_list in
    let update1 from_station to_station = 
        let ekikan_kyori = get_global_ekikan_kyori from_station.name to_station.name in
        let new_distance = from_station.shortest_distance_km +. ekikan_kyori in
            if ekikan_kyori != infinity && new_distance < to_station.shortest_distance_km
                then {name=to_station.name; shortest_distance_km=new_distance; path=to_station.name :: from_station.path}
                else to_station
    in List.map (update1 from_station) lst

(* tests *)
let t1_1 = update yoyogiuehara [] = []
let t1_2 = update yoyogiuehara [yoyogikouen; meijijinguumae] = [yoyogikouen_updated; meijijinguumae]

(* 14.13 *)
(* 無名関数ver. *)
(* 目的: 基点となる駅と駅のリストを受け取り、更新されたリストを返す *)
let update_with_lambda from_station lst = 
    List.map
        (fun to_station ->
            let ekikan_kyori = get_ekikan_kyori global_station_edge_list from_station.name to_station.name in
            let new_distance = from_station.shortest_distance_km +. ekikan_kyori in
                if ekikan_kyori != infinity && new_distance < to_station.shortest_distance_km
                    then {name=to_station.name; shortest_distance_km=new_distance; path=to_station.name :: from_station.path}
                    else to_station)
        lst

let t2_1 = update_with_lambda yoyogiuehara [] = []
let t2_2 = update_with_lambda yoyogiuehara [yoyogikouen; meijijinguumae] = [yoyogikouen_updated; meijijinguumae]
