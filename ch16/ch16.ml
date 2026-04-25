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
