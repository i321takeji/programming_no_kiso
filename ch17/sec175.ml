(* sec17.5 *)
type 'a tree_t =
  | Empty (* 空の木 *)
  | Leaf of 'a (* 葉*)
  | Node of 'a tree_t * 'a * 'a tree_t (* 節 *)

(* 木の例 *)
let tree1 = Empty

let tree2 = Leaf 3

let tree3 = Node (tree1, 4, tree2)

let tree4 = Node (tree2, 5, tree3)

(* exer17.9 *)
(* 目的：多層の木の肩を使って 17.3 節で作った関数 sum_tree を定義 *)
(* sum_tree : int tree_t -> int *)
let rec sum_tree tree =
  match tree with
  | Empty -> 0
  | Leaf n -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2

(* テスト *)
let sum_tree_test1 = sum_tree tree1 = 0

let sum_tree_test2 = sum_tree tree2 = 3

let sum_tree_test3 = sum_tree tree3 = 7

let sum_tree_test4 = sum_tree tree4 = 15

type ('a, 'b) assoc_tree_t =
  | Empty (* 空の木 *)
  | Leaf of 'a * 'b (* 葉*)
  | Node of ('a, 'b) assoc_tree_t * 'a * 'b * ('a, 'b) assoc_tree_t (* 節 *)
