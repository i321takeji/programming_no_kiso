(* exer 4.6 *)
(* 目的：鶴の数 t を与えたら，足の本数を計算する *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi t = t * 2

(* テスト *)
let test_t1 = tsuru_no_ashi 1 = 2
let test_t2 = tsuru_no_ashi 10 = 20
let test_t3 =  tsuru_no_ashi 23 = 46


(* 目的：亀の数 k を与えたら，足の本数を計算する *)
(* kame_no_ashi : int -> int *)
let kame_no_ashi k = k * 4

(* テスト *)
let test_k1 = kame_no_ashi 1 = 4
let test_k2 = kame_no_ashi 10 = 40
let test_k3 =  kame_no_ashi 23 = 92

(* exer 4.7 *)
(* 目的：鶴の数 t と亀の数 k を与えたら，足の本数の合計を計算する *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi t k = tsuru_no_ashi t + kame_no_ashi k

(* テスト *)
let test_tk1 = tsurukame_no_ashi 1 1 = 6
let test_tk2 = tsurukame_no_ashi 5 7 = 38
let test_tk3 =  tsurukame_no_ashi 10 20 = 100

(* exer 4.8 *)
(* 目的：鶴と亀の数の合計 tk と足の合計 l を与えたら，【亀の数】を計算する *)
(* tsurukame_k : int -> int -> int *)
let tsurukame_k tk l = (l - 2 * tk) / 2

(* テスト *)
let test_tk_k1 = tsurukame_k 2 6 = 1
let test_tk_k2 = tsurukame_k 5 16 = 3
let test_tk_k3 =  tsurukame_k 10 32 = 6

(* exer 4.8 *)
(* 目的：鶴と亀の数の合計 tk と足の合計 l を与えたら，【鶴の数】を計算する *)
(* tsurukame_t : int -> int -> int *)
let tsurukame_t tk l = (4 * tk - l) / 2

let o_test1 = tsurukame_t 3 10 = 1
let o_test2 = tsurukame_t 6 18 = 3
let y_test1 = tsurukame_t 0 0 = 0
let y_test2 = tsurukame_t 1 4 = 0
let y_test3 = tsurukame_t 1 2 = 1
let y_test4 = tsurukame_t 2 8 = 0
let y_test5 = tsurukame_t 2 6 = 1
let y_test6 = tsurukame_t 2 4 = 2
let y_test7  = tsurukame_t 3 12 = 0
let y_test8  = tsurukame_t 3 10 = 1
let y_test9  = tsurukame_t 3 8  = 2
let y_test10 = tsurukame_t 3 6  = 3