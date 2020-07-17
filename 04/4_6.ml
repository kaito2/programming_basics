(* 4.6 *)
(* 目的: 鶴の数から足の数を算出する *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi num = num * 2

(* tests *)
let t1 = tsuru_no_ashi 0 = 0
let t2 = tsuru_no_ashi 1 = 2
let t3 = tsuru_no_ashi 256 = 512


(* 4.7 *)
(* 目的: 鶴の数と亀の数から足の数を算出する *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi tsuru_num kame_num = tsuru_num * 2 + kame_num * 4

(* tests *)
let t1 = tsurukame_no_ashi 0 0 = 0
let t2 = tsurukame_no_ashi 1 2 = 10
let t3 = tsurukame_no_ashi 2 1 = 8


(* 4.8 *)
(* 目的: 鶴亀算を解く *)
(* tsurukame : int -> int -> int *)
let tsurukame tsurukame_num legs_num = ( tsurukame_num * 4 - legs_num ) / 2 

(* tests *)               
let t1 = tsurukame 11 38 = 3
let t2 = tsurukame 11 28 = 8
let t1 = tsurukame 0 0 = 0