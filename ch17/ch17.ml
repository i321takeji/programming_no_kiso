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

(* exer17.1 *)

(* 目的：誕生年と現在の年を nengou_t 型の値として受け取て来たら，年齢を返す関数 nenrei *)
(* nenrei : nengou_t -> nengou_t -> int *)
let nenrei birth_nengou current_nengou =
  let birth_seireki = to_seireki birth_nengou in
  let current_seireki = to_seireki current_nengou in
  current_seireki - birth_seireki

(* テスト *)
(* ex17.1 *)
let nenrei_test1 = nenrei (Showa 63) (Heisei 1) = 1

let nenrei_test2 = nenrei (Heisei 1) (Heisei 1) = 0

let nenrei_test3 = nenrei (Meiji 45) (Taisho 1) = 0

let nenrei_test4 = nenrei (Taisho 15) (Showa 1) = 0

let nenrei_test5 = nenrei (Meiji 40) (Heisei 30) = 111

let nenrei_test6 = nenrei (Taisho 10) (Heisei 30) = 97

let nenrei_test7 = nenrei (Showa 50) (Heisei 30) = 43

let nenrei_test8 = nenrei (Heisei 30) (Heisei 30) = 0

(* exer17.2 *)
(* 目的：1 月から 12 月までを表す構成子 January, ..., December を持つ型 year_t を宣言．
   構成子には日を示す引数を取る． *)
type year_t =
  | January of int
  | February of int
  | March of int
  | April of int
  | May of int
  | June of int
  | July of int
  | August of int
  | September of int
  | October of int
  | November of int
  | December of int

(* exer17.3 *)
(* 12 星座を示す型 seiza_t を宣言 *)
type seiza_t =
  | Ohitsujiza
  | Oushiza
  | Futagoza
  | Kaniza
  | Shishiza
  | Otomeza
  | Tenbinza
  | Sasoriza
  | Iteza
  | Yagiza
  | Mizugameza
  | Uoza

(* exer17.4 *)
(* 目的：year_t 型の値を受け取ってきたら，seiza_t 型の星座を返す関数 seiza *)
(* seiza : year_t -> seiza_t *)
let seiza year =
  match year with
  | January d ->
      if 1 <= d && d <= 19 then
        Yagiza
      else if d <= 31 then
        Mizugameza
      else
        failwith "bad day"
  | February d ->
      if 1 <= d && d <= 18 then
        Mizugameza
      else if d <= 28 then
        Uoza
      else
        failwith "bad day"
  | March d ->
      if 1 <= d && d <= 20 then
        Uoza
      else if d <= 31 then
        Ohitsujiza
      else
        failwith "bad day"
  | April d ->
      if 1 <= d && d <= 19 then
        Ohitsujiza
      else if d <= 30 then
        Oushiza
      else
        failwith "bad day"
  | May d ->
      if 1 <= d && d <= 20 then
        Oushiza
      else if d <= 31 then
        Futagoza
      else
        failwith "bad day"
  | June d ->
      if 1 <= d && d <= 21 then
        Futagoza
      else if d <= 30 then
        Kaniza
      else
        failwith "bad day"
  | July d ->
      if 1 <= d && d <= 22 then
        Kaniza
      else if d <= 31 then
        Shishiza
      else
        failwith "bad day"
  | August d ->
      if 1 <= d && d <= 22 then
        Shishiza
      else if d <= 31 then
        Otomeza
      else
        failwith "bad day"
  | September d ->
      if 1 <= d && d <= 22 then
        Otomeza
      else if d <= 30 then
        Tenbinza
      else
        failwith "bad day"
  | October d ->
      if 1 <= d && d <= 23 then
        Tenbinza
      else if d <= 31 then
        Sasoriza
      else
        failwith "bad day"
  | November d ->
      if 1 <= d && d <= 22 then
        Sasoriza
      else if d <= 30 then
        Iteza
      else
        failwith "bad day"
  | December d ->
      if 1 <= d && d <= 21 then
        Iteza
      else if d <= 31 then
        Yagiza
      else
        failwith "bad day"

(* テスト *)
let seiza_test1 = seiza (March 21) = Ohitsujiza

let seiza_test2 = seiza (April 19) = Ohitsujiza

let seiza_test3 = seiza (April 20) = Oushiza

let seiza_test4 = seiza (May 20) = Oushiza

let seiza_test5 = seiza (May 21) = Futagoza

let seiza_test6 = seiza (June 21) = Futagoza

let seiza_test7 = seiza (June 22) = Kaniza

let seiza_test8 = seiza (July 22) = Kaniza

let seiza_test9 = seiza (July 23) = Shishiza

let seiza_test10 = seiza (August 22) = Shishiza

let seiza_test11 = seiza (August 23) = Otomeza

let seiza_test12 = seiza (September 22) = Otomeza

let seiza_test13 = seiza (September 23) = Tenbinza

let seiza_test14 = seiza (October 23) = Tenbinza

let seiza_test15 = seiza (October 24) = Sasoriza

let seiza_test16 = seiza (November 22) = Sasoriza

let seiza_test17 = seiza (November 23) = Iteza

let seiza_test18 = seiza (December 21) = Iteza

let seiza_test19 = seiza (December 22) = Yagiza

let seiza_test20 = seiza (January 19) = Yagiza

let seiza_test21 = seiza (January 20) = Mizugameza

let seiza_test22 = seiza (February 18) = Mizugameza

let seiza_test23 = seiza (February 19) = Uoza

let seiza_test24 = seiza (March 20) = Uoza

(* sec17.2 *)

(* 木を表す型 *)
type tree_t =
  | Empty (* 空の木 *)
  | Leaf of int (* 葉 *)
  | Node of tree_t * int * tree_t (* 節 *)

(* 節のみの木を表す型 *)
type tree_node_t = OnlyNode of tree_node_t * int * tree_node_t

let rec tn1 = OnlyNode (tn1, 0, tn1)

(* sec17.3 *)

(* 木の例 *)
let tree1 = Empty

let tree2 = Leaf 3

let tree3 = Node (tree1, 4, tree2)

let tree4 = Node (tree2, 5, tree3)

(* 目的：tree に含まれる整数をすべて加える *)
(* sum_tree : tree_t -> int *)
let rec sum_tree tree =
  match tree with
  | Empty -> 0
  | Leaf n -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2

(* テスト *)
let test1 = sum_tree tree1 = 0

let test2 = sum_tree tree2 = 3

let test3 = sum_tree tree3 = 7

let test4 = sum_tree tree4 = 15

(* exer17.5 *)
(* 目的：tree_t 型の木を受け取ったら，節や葉に入っている値をすべて 2 倍した木を返す関数 tree_double *)
(* tree_double : tree_t -> int *)
let rec tree_double tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node (tree_double t1, n * 2, tree_double t2)

(* テスト *)
let tree_double_test1 = tree_double tree1 = Empty

let tree_double_test2 = tree_double tree2 = Leaf 6

let tree_double_test3 = tree_double tree3 = Node (Empty, 8, Leaf 6)

let tree_double_test4 =
  tree_double tree4 = Node (Leaf 6, 10, Node (Empty, 8, Leaf 6))

(* exer17.6 *)
(* 目的：int -> int 型の関数 f と tree_t 型の木を受け取ったら，節や葉に入っている値すべてに f を適用した木を返す関数 tree_map *)
(* tree_map : (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2)

(* テスト *)
let tree_map_test1 = tree_map (( * ) 2) tree1 = Empty

let tree_map_test2 = tree_map (( * ) 2) tree2 = Leaf 6

let tree_map_test3 = tree_map (( * ) 2) tree3 = Node (Empty, 8, Leaf 6)

let tree_map_test4 =
  tree_map (( * ) 2) tree4 = Node (Leaf 6, 10, Node (Empty, 8, Leaf 6))

(* exer17.7 *)
(* 目的：tree_t 型の木を受け取ったら，節と葉が合計いくつあるかを返す関数 tree_length *)
(* tree_length : tree_t -> int *)
let rec tree_length tree =
  match tree with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (t1, _, t2) -> 1 + tree_length t1 + tree_length t2

(* テスト *)
let tree_length_test1 = tree_length tree1 = 0

let tree_length_test2 = tree_length tree2 = 1

let tree_length_test3 = tree_length tree3 = 2

let tree_length_test4 = tree_length tree4 = 4

(* exer17.8 *)
(* 目的：tree_t 型の木を受け取ったら，木の深さを返す関数 tree_depth *)
(* tree_depth : tree_t -> int *)
let rec tree_depth tree =
  match tree with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (t1, _, t2) -> 1 + max (tree_depth t1) (tree_depth t2)

(* テスト *)
let tree_depth_test1 = tree_depth tree1 = 0

let tree_depth_test2 = tree_depth tree2 = 1

let tree_depth_test3 = tree_depth tree3 = 2

let tree_depth_test4 = tree_depth tree4 = 3

(* sec17.4 *)

(* 目的：data が 2 分探索木 tree に含まれているかを調べる *)
(* search : tree_t -> int -> bool *)
let rec search tree data =
  match tree with
  | Empty -> false
  | Leaf n -> data = n
  | Node (t1, n, t2) ->
      if data = n then
        true
      else if data < n then
        search t1 data
      else
        search t2 data

(* 2 分探索木の例 *)
let tree1 = Empty

let tree2 = Leaf 3

let tree3 = Node (Leaf 1, 2, Leaf 3)

let tree4 = Node (Empty, 7, Leaf 9)

let tree5 = Node (tree3, 6, tree4)

(* テスト *)
let test1 = search tree1 3 = false

let test2 = search tree2 3 = true

let test3 = search tree2 4 = false

let test4 = search tree5 6 = true

let test5 = search tree5 2 = true

let test6 = search tree5 1 = true

let test7 = search tree5 4 = false

let test8 = search tree5 7 = true

let test9 = search tree5 8 = false

(* 目的：2 分探索木 tree に data を追加した 2 分探索木を返す *)
(* insert_tree : tree_t -> int -> tree_t *)
let rec insert_tree tree data =
  match tree with
  | Empty -> Leaf data
  | Leaf n ->
      if data = n then
        Leaf n
      else if data < n then
        Node (Leaf data, n, Empty)
      else
        Node (Empty, n, Leaf data)
  | Node (t1, n, t2) ->
      if data = n then
        Node (t1, n, t2)
      else if data < n then
        Node (insert_tree t1 data, n, t2)
      else
        Node (t1, n, insert_tree t2 data)

(* テスト *)
let test1 = insert_tree Empty 3 = Leaf 3

let test2 = insert_tree (Leaf 3) 2 = Node (Leaf 2, 3, Empty)

let test3 = insert_tree (Leaf 3) 3 = Leaf 3

let test4 = insert_tree (Leaf 3) 4 = Node (Empty, 3, Leaf 4)

let test5 =
  insert_tree tree5 4
  = Node
      (Node (Leaf 1, 2, Node (Empty, 3, Leaf 4)), 6, Node (Empty, 7, Leaf 9))

(* exer17.17 *)
(* 目的：10.2 節の minimum を，最小値の候補とそのほかの空かも知れないリスト (rest) を別々に取るように変更 *)
(* minimum : int list -> int *)
let minimum lst =
  let rec minimum_sub v lst =
    match lst with
    | [] -> v
    | first :: rest ->
        let c = minimum_sub first rest in
        if v < c then
          v
        else
          c
  in
  match lst with
  | [] -> max_int
  | first :: rest -> minimum_sub first rest

(* テスト *)
let test1_minimum = minimum [3] = 3

let test2_minimum = minimum [1; 2] = 1

let test3_minimum = minimum [3; 2] = 2

let test4_minimum = minimum [3; 2; 6; 4; 1; 8] = 1
