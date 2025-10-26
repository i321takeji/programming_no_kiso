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
(* 目的：鶴と亀の数の合計 tk と足の合計 l を与えたら，亀の数を計算する *)
(* tsurukame : int -> int -> int *)
let tsurukame tk l = (l - 2 * tk) / 2

(* テスト *)
let test_tk1 = tsurukame 2 6 = 1
let test_tk2 = tsurukame 5 16 = 3
let test_tk3 =  tsurukame 10 32 = 6
