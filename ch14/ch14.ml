(* 目的：受け取ったリスト lst から正の要素のみを取り出す *)
(* filter_positive : int list -> int list *)
let rec filter_positive lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if first > 0 then
        first :: filter_positive rest
      else
        filter_positive rest

(* 目的：整数 n が 3 割ると 1 余るかを調べる *)
(* is_mod3_1 : int -> bool *)
let is_mod3_1 n = n mod 3 = 1

(* 目的：リスト lst から 3 で割ると 1 余る要素のみを取り出す *)
(* filter_mod3_1 : int list -> int list *)
let rec filter_mod3_1 lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if is_mod3_1 first then
        first :: filter_mod3_1 rest
      else
        filter_mod3_1 rest

(* 目的：リスト lst の中から条件 p を満たす要素のみ取り出す *)
(* filter : ('a -> bool) -> 'a list -> 'a list *)
let rec filter p lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if p first then
        first :: filter p rest
      else
        filter p rest

(* 目的：リスト lst から 3 で割ると 1 余る要素のみを取り出す *)
(* filter_mod3_1 : int list -> int list *)
let filter_mod3_1 lst = filter is_mod3_1 lst

(* 目的：整数 n が正かどうかを調べる *)
(* is_positive : int -> bool *)
let is_positive n = n > 0

(* 目的：受け取ったリスト lst から正の要素のみを取り出す *)
(* filter_positive : int list -> int list *)
let filter_positive lst = filter is_positive lst

(* exer14.1 *)
(* 目的：問題 9.5 で作成した even を filter を用いて定義 *)
(* even : int list -> int list *)
let even lst =
  let is_even n = n mod 2 = 0 in
  List.filter is_even lst

let test_even1 = even [] = []

let test_even2 = even [1; 3; 5] = []

let test_even3 = even [2; 4; 6] = [2; 4; 6]

let test_even4 = even [1; 2; 3; 4; 5] = [2; 4]

let test_even5 = even [-2; -1; 0; 7; 8] = [-2; 0; 8]

(* section 9.6 *)
type gakusei_t =
  { namae : string (* 名前 *)
  ; tensuu : int (* 点数 *)
  ; seiseki : string (* 成績 *)
  }

(* gakusei_t list 型のデータの例 *)
let g1 = { namae = "asai"; tensuu = 70; seiseki = "B" }

let g2 = { namae = "kaneko"; tensuu = 85; seiseki = "A" }

let g3 = { namae = "yoshida"; tensuu = 80; seiseki = "A" }

let g4 = { namae = "tanaka"; tensuu = 92; seiseki = "S" }

(* exer14.2 *)
(* 目的：問題 9.6 で作成した count_A を filter と length を用いて定義 *)
(* count_A : gakusei_t list -> int *)
let count_A lst =
  let is_A gakusei = gakusei.seiseki = "A" in
  List.length (List.filter is_A lst)

(* テスト *)
let test_count_A1 = count_A [] = 0

let test_count_A2 = count_A [g1] = 0

let test_count_A3 = count_A [g2] = 1

let test_count_A4 = count_A [g1; g2; g3] = 2

let test_count_A5 = count_A [g1; g4] = 0

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let rec sum lst =
  match lst with
  | [] -> 0
  | first :: rest -> first + sum rest

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let rec length lst =
  match lst with
  | [] -> 0
  | first :: rest -> 1 + length rest

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | first :: rest -> first :: append rest lst2

(* 目的：init から始めて lst の要素を右から順に f を施し込む *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init =
  match lst with
  | [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* 目的：first と rest_result を加える *)
(* add_int : int -> int -> int *)
let add_int first rest_result = first + rest_result

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right add_int lst 0

(* 目的：first は無視して rest_result に 1 を加える *)
(* add_one : int -> int -> int *)
let add_one first rest_result = 1 + rest_result

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let length lst = fold_right add_one lst 0

(* 目的：first をリスト rest_result の先頭に加える *)
(* cons : 'a -> 'a list -> 'a list *)
let cons first rest_result = first :: rest_result

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = fold_right cons lst1 lst2

(* exer14.3 *)
(* 目的：問題 9.6 で作成した関数 concat を fold_right を使って書き直す *)
(* concat : string list -> string *)
let concat lst = fold_right ( ^ ) lst ""

let test_concat1 = concat [] = ""

let test_concat2 = concat ["a"] = "a"

let test_concat3 = concat ["ab"; "cd"] = "abcd"

let test_concat4 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

let test_concat5 = concat ["a"; ""; "b"; ""] = "ab"

(* exer14.4 *)
(* 目的：gakusei_t 型のリストを受け取ったら，全員の得点の合計を返す関数 *)
(* gakusei_sum gakusei_t list -> int *)
let gakusei_sum lst =
  let add_gakusei_tensuu gakusei rest_result =
    gakusei.tensuu + rest_result
  in
  fold_right add_gakusei_tensuu lst 0

(* テスト *)

let test_gakusei_sum1 = gakusei_sum [] = 0

let test_gakusei_sum2 = gakusei_sum [g1] = g1.tensuu

let test_gakusei_sum3 = gakusei_sum [g2] = g2.tensuu

let test_gakusei_sum4 =
  gakusei_sum [g1; g2; g3] = g1.tensuu + g2.tensuu + g3.tensuu

let test_gakusei_sum5 = gakusei_sum [g1; g4] = g1.tensuu + g4.tensuu

(* sec14.3 *)

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst =
  (* 目的：first と rest_result を加える *)
  (* add_int : int -> int -> int *)
  let add_int first rest_result = first + rest_result in
  fold_right add_int lst 0

(* exer14.5 *)
(* even, count_A, gakusei_sum の書き換えは，すでに完了
  concat は対象外
 *)

(* exer14.6 *)
(* 目的：学生リスト lst のうち成績が seiseki0 の人の数を返す *)
(* count : gakusei_t list -> string -> int *)
let count lst seiseki0 =
  let is_seiseki0 gakusei = gakusei.seiseki = seiseki0 in
  List.length (List.filter is_seiseki0 lst)

let test_count_1 = count [] "A" = 0

let test_count_2 = count [g1] "A" = 0

let test_count_3 = count [g2] "A" = 1

let test_count_4 = count [g1; g2; g3; g4] "A" = 2

let test_count_5 = count [g1; g2; g3; g4] "S" = 1

(* sec14.4 *)
(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right (fun first rest_result -> first + rest_result) lst 0

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let length lst = fold_right (fun first rest_result -> 1 + rest_result) lst 0

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 =
  fold_right (fun first result_result -> first :: result_result) lst1 lst2

(* exer14.8 *)
let ex14_8 = fun x -> (x * x) - 1

let test_ex14_8_1 = ex14_8 0 = -1

let test_ex14_8_2 = ex14_8 1 = 0

let test_ex14_8_3 = ex14_8 2 = 3

let test_ex14_8_4 = ex14_8 5 = 24

let test_ex14_8_5 = ex14_8 (-3) = 8

(* exer14.9 *)

(* exer 8.3 より *)
(* 人と名前，身長 (m)，体重 (kg)，誕生日 (月と日)，血液型を表す型 *)
type person_t =
  { name : string
  ; height : float
  ; weight : float
  ; birth : int * int
  ; blood : string
  }

let ex14_9 = fun p -> p.name

let p1 =
  { name = "yamada"
  ; height = 1.7
  ; weight = 60.0
  ; birth = (1, 1)
  ; blood = "A"
  }

let p2 =
  { name = "tanaka"
  ; height = 1.6
  ; weight = 50.0
  ; birth = (12, 31)
  ; blood = "B"
  }

let p3 =
  { name = ""; height = 1.8; weight = 70.0; birth = (6, 15); blood = "O" }

let test_ex14_9_1 = ex14_9 p1 = "yamada"

let test_ex14_9_2 = ex14_9 p2 = "tanaka"

let test_ex14_9_3 = ex14_9 p3 = ""

(* exer14.10 *)

(* even : int list -> int list *)
let even lst = List.filter (fun n -> n mod 2 = 0) lst

let test_even1 = even [] = []

let test_even2 = even [1; 3; 5] = []

let test_even3 = even [2; 4; 6] = [2; 4; 6]

let test_even4 = even [1; 2; 3; 4; 5] = [2; 4]

let test_even5 = even [-2; -1; 0; 7; 8] = [-2; 0; 8]

(* count_A : gakusei_t list -> int *)
let count_A lst =
  List.length (List.filter (fun gakusei -> gakusei.seiseki = "A") lst)

(* テスト *)
let test_count_A1 = count_A [] = 0

let test_count_A2 = count_A [g1] = 0

let test_count_A3 = count_A [g2] = 1

let test_count_A4 = count_A [g1; g2; g3] = 2

let test_count_A5 = count_A [g1; g4] = 0

(* gakusei_sum gakusei_t list -> int *)
let gakusei_sum lst =
  fold_right (fun gakusei rest_result -> gakusei.tensuu + rest_result) lst 0

(* テスト *)

let test_gakusei_sum1 = gakusei_sum [] = 0

let test_gakusei_sum2 = gakusei_sum [g1] = g1.tensuu

let test_gakusei_sum3 = gakusei_sum [g2] = g2.tensuu

let test_gakusei_sum4 =
  gakusei_sum [g1; g2; g3] = g1.tensuu + g2.tensuu + g3.tensuu

let test_gakusei_sum5 = gakusei_sum [g1; g4] = g1.tensuu + g4.tensuu

(* exer14.11 *)
