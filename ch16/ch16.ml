(* 距離と距離の合計を持っている型 *)
type distance_t = { kyori : float (* 距離 *); total : float (* 距離の合計 *) }

(* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
(* total_distance : distance_t list -> distance_t list *)
let total_distance lst =
  (* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
  (* ここで total0 はこれまでの距離の合計 *)
  (* hojo : distance_t list -> float -> distance_t list *)
  let rec hojo lst total0 =
    match lst with
    | [] -> []
    | { kyori = k; total = t } :: rest ->
        { kyori = k; total = total0 +. k } :: hojo rest (total0 +. k)
  in
  hojo lst 0.0

(* exer16.1 *)
(* 目的：整数のリストを受け取ったら，それまでの数の合計からなるリストを返す関数 *)
(* sum_list : int list -> int list *)
let sum_list lst =
  let rec hojo lst total0 =
    match lst with
    | [] -> []
    | first :: rest ->
        let acc = total0 + first in
        acc :: hojo rest acc
  in
  hojo lst 0

(* テストケース *)
let test1_sum_list = sum_list [] = []

let test2_sum_list = sum_list [3] = [3]

let test3_sum_list = sum_list [3; 2] = [3; 5]

let test4_sum_list = sum_list [3; 2; 1] = [3; 5; 6]

let test5_sum_list = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]

(* sec16.3 *)
(* 目的：与えられたリストを逆順にして返す *)
(* reverse : 'a list -> 'a list *)
let reverse lst =
  (* 目的：(lst の逆順リスト) @ result を返す *)
  (* ここで result はこれまでの要素を逆順にしたリストを返す *)
  let rec rev lst result =
    match lst with
    | [] -> result
    | first :: rest -> rev rest (first :: result)
  in
  rev lst []

(* exer16.2 *)
(* 目的：関数 f と初期値 init，そしてリスト lst を受け取ったら，
  init からはじめてリスト lst の要素を「左から」順に f を施し込む関数 fold_left を作る *)
(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f init lst =
  match lst with
  | [] -> init
  | first :: rest -> fold_left f (f init first) rest

let test1 = fold_left ( + ) 0 [] = 0

let test2 = fold_left ( + ) 0 [1; 2; 3] = 6

let test3 = fold_left (fun acc x -> acc - x) 0 [1; 2; 3] = -6

let test4 = fold_left (fun acc x -> x :: acc) [] [1; 2; 3] = [3; 2; 1]

let test5 = fold_left ( ^ ) "" ["a"; "b"; "c"] = "abc"
