(* #use "station.ml" *)
(* #use "12.ml" *)

(* 目的: 「最短距離最小の駅」と「最短距離理最小以外の駅からなるリスト」を返す *)
(* separate_min_shortest_station : station_t list -> station_t * station_t list *)
let separate_min_shortest_station station_lst = 
    let iter station provisional_pair = match provisional_pair with 
        (prov_min_station, prov_list) -> 
            if station.shortest_distance_km < prov_min_station.shortest_distance_km
                then (station, prov_min_station :: prov_list)
                else (prov_min_station, station :: prov_list)
    in match station_lst with
        (* TODO: Error hadling... *)
          [] -> ({name="Empty station list is given."; shortest_distance_km=infinity; path=[]}, [])
        | first :: rest -> 
            List.fold_right iter rest (first, [])

(* tests *)
let given_station_node_list = [
    {name="代々木上原"; shortest_distance_km=infinity; path=[]};
    {name="代々木公園"; shortest_distance_km=0.; path=["代々木公園"]};
    {name="明治神宮前"; shortest_distance_km=infinity; path=[]};
]
let t1 = separate_min_shortest_station given_station_node_list
    = (
        {name="代々木公園"; shortest_distance_km=0.; path=["代々木公園"]},
        [
            {name="代々木上原"; shortest_distance_km=infinity; path=[]};
            {name="明治神宮前"; shortest_distance_km=infinity; path=[]};
        ]
    )
