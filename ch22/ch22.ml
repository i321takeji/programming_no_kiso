(* 目的：フィボナッチ数を再帰回数とともに求める *)
(* fib : int -> int -> (int * int) *)
let rec fib n c =
  (* c はこれまでに呼ばれた回数*)
  let c0 = c + 1 in
  (* カウンタに 1 を加える*)
  if n < 2 then
    (n, c0)
    (* カウンタを一緒に返す *)
  else
    let (r1, c1) = fib (n - 1) c0 in
    (* c0 からはじめて fib (n - 1) 中での呼び出し回数を数える *)
    let (r2, c2) = fib (n - 2) c1 in
    (* c1 からはじめて fib (n - 2) 中での呼び出し回数を数える *)
    (r1 + r2, c2)
(* c2 が全体の呼び出し回数*)

(* exer22.1 *)
let count = ref 0

(* gensym : string -> string *)
let gensym str =
  let n = !count in
  count := !count + 1 ;
  str ^ string_of_int n

(* exer22.2 *)
(* fib_array : int array -> int array *)
let fib_array arr =
  let len = Array.length arr in
  let rec fib n =
    if n < len then (
      if n = 0 then
        arr.(0) <- 0
      else if n = 1 then
        arr.(1) <- 1
      else
        arr.(n) <- arr.(n - 1) + arr.(n - 2) ;
      fib (n + 1) )
    else
      ()
  in
  fib 0 ; arr

(* テスト *)
let fin_array_test1 =
  fib_array [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
  = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|]
