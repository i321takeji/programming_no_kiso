(* sec20.1 *)

(* 赤か黒を示す型 *)
type color_t = Red | Black

(* exer20.1 *)

type ('a, 'b) rb_tree_t =
  | Empty
  | Node of
      ('a, 'b) rb_tree_t
      * (* キー*)
      'a
      * (* 値 *)
      'b
      * (* 色 *)
        color_t
      * ('a, 'b) rb_tree_t

(* exer20.2 *)
(* 目的：rb_tree_t 型の木を受け取ったら，その木が現在の頂点が黒で，子と孫が赤であるかを調べ，
   そうなっていたら，赤が連続しないように，かつ，赤を頂点に変更した木を返す *)
(* blance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let blance rb_tree =
  match rb_tree with
  | Node (Node (Node (a, xk, xv, Red, b), yk, yv, Red, c), zk, zv, Black, d)
  | Node (Node (a, xk, xv, Red, Node (b, yk, yv, Red, c)), zk, zv, Black, d)
  | Node (a, xk, xv, Black, Node (Node (b, yk, yv, Red, c), zk, zv, Red, d))
  | Node (a, xk, xv, Black, Node (b, yk, yv, Red, Node (c, zk, zv, Red, d)))
    ->
      Node
        (Node (a, xk, xv, Black, b), yk, yv, Red, Node (c, zk, zv, Black, d))
  | _ -> rb_tree

(* テスト *)
let ta = Node (Empty, "a_k", "a_v", Black, Empty)

let tb = Node (Empty, "b_k", "b_v", Black, Empty)

let tc = Node (Empty, "c_k", "c_v", Black, Empty)

let td = Node (Empty, "d_k", "d_v", Black, Empty)

let fig2021 =
  Node
    ( Node (Node (ta, "x_k", "x_v", Red, tb), "y_k", "y_v", Red, tc)
    , "z_k"
    , "z_v"
    , Black
    , td )

let fig2022 =
  Node
    ( Node (ta, "x_k", "x_v", Red, Node (tb, "y_k", "y_v", Red, tc))
    , "z_k"
    , "z_v"
    , Black
    , td )

let fig2023 =
  Node
    ( ta
    , "x_k"
    , "x_v"
    , Black
    , Node (Node (tb, "y_k", "y_v", Red, tc), "z_k", "z_v", Red, td) )

let fig2024 =
  Node
    ( ta
    , "x_k"
    , "x_v"
    , Black
    , Node (tb, "y_k", "y_v", Red, Node (tc, "z_k", "z_v", Red, td)) )

(* テストはあと回し *)

(* exer20.3 *)
(* 目的：赤黒木とキーと値を受け取ったら，それを挿入した赤黒木を返す関数．
   挿入するキーがすでに赤黒木に存在する場合には，その頂点の値を新しく挿入する値で置き換える． *)
(* insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let rec insert rb_tree k v =
  let rec insert_sub rb_tree k v =
    match rb_tree with
    | Empty -> Node (Empty, k, v, Red, Empty)
    | Node (left, key, value, rb, right) ->
        if k = key then
          Node (left, key, v, rb, right)
        else if k < key then
          blance (Node (insert_sub left k v, key, value, rb, right))
        else
          blance (Node (left, key, value, rb, insert_sub right k v))
  in
  match insert_sub rb_tree k v with
  | Empty -> assert false
  | Node (left, key, value, _, right) -> Node (left, key, value, Black, right)

(* テストは後回し *)

(* set20.5 *)

(* 空の赤黒木 *)
let empty = Empty

(* exer20.4 *)
(* 目的：赤黒木とキーを受け取ったら，そのキーに対応する値を赤黒木の中から探す関数 *)
(* search : ('a, 'b) rb_tree_t -> 'a -> 'b *)
let rec search rb_tree k =
  match rb_tree with
  | Empty -> raise Not_found
  | Node (left, key, value, _, right) ->
      if k = key then
        value
      else if k < key then
        search left k
      else
        search right k

(* テストは後回し *)
