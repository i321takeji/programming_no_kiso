(* sec17.2 *)

(* 木を表す型 *)
type tree_t =
  | Empty (* 空の木 *)
  | Leaf of int (* 葉 *)
  | Node of tree_t * int * tree_t (* 節 *)

(* 節のみの木を表す型 *)
type tree_node_t = OnlyNode of tree_node_t * int * tree_node_t

let rec tn1 = OnlyNode (tn1, 0, tn1)
