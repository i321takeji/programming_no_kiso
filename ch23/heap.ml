(* sec23.7 *)

(* ヒープの添字の型 *)
type index_t = int ref

(* 最小値を求める値が 'a で そのほかの付加情報が 'b 型であるヒープの型*)
type ('a, 'b) t = int ref * (index_t * 'a * 'b) array
