(* sec17.1 *)

(* 赤組か白組かを表す型 *)
type team_t = Red | White

(* 目的：受け取ったチーム名を文字列で返す *)
(* team_string : team_t -> string *)
let team_string team =
  match team with
  | Red -> "赤組"
  | White -> "白組"

(* 年号を表す型 *)
type nengou_t =
  | Meiji of int (* 明治 *)
  | Taisho of int (* 大正 *)
  | Showa of int (* 昭和 *)
  | Heisei of int (* 平成 *)

(* 目的：年号を受け取ったら対応する西暦年を返す *)
(* to_seireki : nengou_t -> int *)
let to_seireki nengou =
  match nengou with
  | Meiji n -> n + 1867
  | Taisho n -> n + 1911
  | Showa n -> n + 1925
  | Heisei n -> n + 1988

(* sec17.2 *)

(* 木を表す型 *)
type tree_t =
  | Empty (* 空の木 *)
  | Leaf of int (* 葉 *)
  | Node of tree_t * int * tree_t (* 節 *)

(* 節のみの木を表す型 *)
type tree_node_t = OnlyNode of tree_node_t * int * tree_node_t

let rec tn1 = OnlyNode (tn1, 0, tn1)
