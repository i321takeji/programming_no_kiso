(* sec10.1 *)

(* 目的：受け取った lst の要素それぞれの先頭に n をくっつける *)
(* add_to_each : int -> int list list -> int list list *)
let rec add_to_each n lst =
  match lst with
  | [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest

(* テスト *)
let test1 = add_to_each 1 [] = []

let test2 = add_to_each 1 [[2]] = [[1; 2]]

let test3 = add_to_each 1 [[2]; [2; 3]] = [[1; 2]; [1; 2; 3]]

let test4 =
  add_to_each 1 [[2]; [2; 3]; [2; 3; 4]] = [[1; 2]; [1; 2; 3]; [1; 2; 3; 4]]

(* 目的：受け取った lst の接頭語のリストを求める *)
(* prefix : int list -> int list list *)
let rec prefix lst =
  match lst with
  | [] -> []
  | first :: rest -> [first] :: add_to_each first (prefix rest)

let test5 = prefix [] = []

let test6 = prefix [1] = [[1]]

let test7 = prefix [1; 2] = [[1]; [1; 2]]

let test8 = prefix [1; 2; 3; 4] = [[1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]]

(* exer10.1 *)

(* 目的：昇順の整数リスト lst と整数 n を受け取り，lst の昇順となる位置に n を挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert lst n =
  match lst with
  | [] -> [n]
  | first :: rest ->
      if first < n then
        first :: insert rest n
      else
        n :: lst

(* テスト *)
let test_insert1 = insert [] 3 = [3]

let test_insert2 = insert [1; 3; 4] 2 = [1; 2; 3; 4]

let test_insert3 = insert [1; 2; 4] 5 = [1; 2; 4; 5]

let test_insert4 = insert [1; 3; 5] 0 = [0; 1; 3; 5]

let test_insert5 = insert [1; 1; 3] 1 = [1; 1; 1; 3]

(* exer10.2 *)

(* 目的：整数のリストを受け取ったら，それを昇順に整列したリストを返す関数 *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> insert (ins_sort rest) first

let test_ins_sort1 = ins_sort [] = []

let test_ins_sort2 = ins_sort [5] = [5]

let test_ins_sort3 = ins_sort [1; 2; 3] = [1; 2; 3]

let test_ins_sort4 = ins_sort [4; 3; 2; 1] = [1; 2; 3; 4]

let test_ins_sort5 = ins_sort [3; 3; 3; 1] = [1; 3; 3; 3]

(* exer10.3 *)
type gakusei_t =
  {namae: string (* 名前 *); tensuu: int (* 点数 *); seiseki: string (* 成績 *)}

(* テスト用データ *)
let g1 = {namae= "taro"; tensuu= 70; seiseki= "B"}

let g2 = {namae= "hanako"; tensuu= 90; seiseki= "A"}

let g3 = {namae= "jiro"; tensuu= 60; seiseki= "C"}

let g4 = {namae= "yuki"; tensuu= 70; seiseki= "B"}

(* 目的：gakusei_t の tensuu の昇順である gakusti_t 型のリスト lst と gakusei_t の値を受け取り，
         lst の tensuu の昇順となる位置に gakusei_t を挿入したリストを返す *)
(* gakusei_insert : gakusei_t list -> gakusei_t -> gakusei_t list *)
let rec gakusei_insert lst gakusei =
  match lst with
  | [] -> [gakusei]
  | ({tensuu} as first) :: rest ->
      if tensuu < gakusei.tensuu then
        first :: gakusei_insert rest gakusei
      else
        gakusei :: lst

let test_gakusei_insert1 = gakusei_insert [] g1 = [g1]

let test_gakusei_insert2 = gakusei_insert [g1] g2 = [g1; g2]

let test_gakusei_insert3 = gakusei_insert [g3; g1] g2 = [g3; g1; g2]

(* 目的：gakusei_t 型のリストを受け取ったら，それを tensuu フィールドの順に整列したリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> gakusei_insert (gakusei_sort rest) first

(* テスト *)
let test_gakusei_sort1 = gakusei_sort [] = []

let test_gakusei_sort2 = gakusei_sort [g1] = [g1]

let test_gakusei_sort3 = gakusei_sort [g1; g2; g3] = [g3; g1; g2]

let test_gakusei_sort4 = gakusei_sort [g2; g1; g3] = [g3; g1; g2]

let test_gakusei_sort5 = gakusei_sort [g1; g4; g3] = [g3; g1; g4]

(* exer10.4 *)
type person_t =
  { name: string
  ; height: float
  ; weight: float
  ; birth: int * int
  ; blood: string }

(* テスト用データ *)
let p1 =
  {name= "sato"; height= 170.0; weight= 60.0; birth= (1, 1); blood= "A"}

let p2 = {name= "abe"; height= 165.0; weight= 55.0; birth= (2, 2); blood= "B"}

let p3 =
  {name= "yamada"; height= 180.0; weight= 70.0; birth= (3, 3); blood= "O"}

let p4 =
  {name= "sato"; height= 172.0; weight= 62.0; birth= (4, 4); blood= "AB"}

(* 目的：person_t の name の昇順である person_t 型のリスト lst と person_t の値を受け取り，
         lst の name の昇順となる位置に person_t を挿入したリストを返す *)
(* person_insert : person_t list -> person_t -> person_t list *)
let rec person_insert lst person =
  match lst with
  | [] -> [person]
  | ({name} as first) :: rest ->
      if name < person.name then
        first :: person_insert rest person
      else
        person :: lst

(* テスト *)
let test_person_insert1 = person_insert [] p1 = [p1]

let test_person_insert2 = person_insert [p1] p2 = [p2; p1]

let test_person_insert3 = person_insert [p2; p1] p3 = [p2; p1; p3]

(* 目的：person_t 型のリストを受け取ったら，それを名前の順に整列したリストを返す *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> person_insert (person_sort rest) first

(* テスト *)
let test_person_sort1 = person_sort [] = []

let test_person_sort2 = person_sort [p1] = [p1]

let test_person_sort3 = person_sort [p1; p2; p3] = [p2; p1; p3]

let test_person_sort4 = person_sort [p3; p1; p2] = [p2; p1; p3]

let test_person_sort5 = person_sort [p1; p4; p2] = [p2; p1; p4]

(* sec10.2 *)
(* 目的：受け取った lst の中の最小値を返す *)
(* minimum : int list -> int *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | first :: rest ->
      if first <= minimum rest then
        first
      else
        minimum rest

let rec minimum' lst =
  match lst with
  | [] -> max_int
  | first :: rest ->
      let min_rest = minimum rest in
      if first <= min_rest then
        first
      else
        min_rest

(* テスト *)
let test1 = minimum [3] = 3

let test2 = minimum [1; 2] = 1

let test3 = minimum [3; 2] = 2

let test4 = minimum [3; 2; 6; 4; 1; 8] = 1

(* exer10.5 *)
(* 目的：gakusei_t 型のリストを受け取ったら，その中の最高得点を受け取った人のレコードを返す *)
(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max lst =
  match lst with
  | [] -> {namae= "nanashi"; tensuu= -1; seiseki= "X"}
  | ({tensuu} as first) :: rest ->
      if tensuu >= (gakusei_max rest).tensuu then
        first
      else
        gakusei_max rest

let test_gakusei_max1 = gakusei_max [g1] = g1

let test_gakusei_max2 = gakusei_max [g1; g2; g3] = g2

let test_gakusei_max3 = gakusei_max [g3; g1; g2] = g2

let test_gakusei_max4 = gakusei_max [g1; g4] = g1 (* 同点なら先に出たものを返す *)

let test_gakusei_max5 = gakusei_max [g4; g1; g3] = g4

(* exer10.6 *)
(* 目的：gakusei_t 型のリストを受け取ったら，その中の最高得点を受け取った人のレコードを返す．局所定義版 *)
(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max' lst =
  match lst with
  | [] -> {namae= "nanashi"; tensuu= -1; seiseki= "X"}
  | ({tensuu} as first) :: rest ->
      let gakusei_max_rest = gakusei_max rest in
      if tensuu >= gakusei_max_rest.tensuu then
        first
      else
        gakusei_max_rest

let test_gakusei_max1 = gakusei_max' [g1] = g1

let test_gakusei_max2 = gakusei_max' [g1; g2; g3] = g2

let test_gakusei_max3 = gakusei_max' [g3; g1; g2] = g2

let test_gakusei_max4 = gakusei_max' [g1; g4] = g1 (* 同点なら先に出たものを返す *)

let test_gakusei_max5 = gakusei_max' [g4; g1; g3] = g4

(* sec10.4 *)
(* 目的：学生リスト lst のうち各成績の人数を集計する *)
(* shukei : gakusei_t list -> int * int * int * int *)
let rec shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | {namae= n; tensuu= t; seiseki= s} :: rest ->
      let a, b, c, d = shukei rest in
      (* match shukei_rest with *)
      (* | a, b, c, d -> *)
      if s = "A" then
        (a + 1, b, c, d)
      else if s = "B" then
        (a, b + 1, c, d)
      else if s = "C" then
        (a, b, c + 1, d)
      else
        (a, b, c, d + 1)

(* exer10.7 *)
(* 目的：person_t 型のリストを受け取ったら，各血液型の人が何人いるかを組みにして返す関数 *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | {blood} :: rest -> (
      let a, b, o, ab = ketsueki_shukei rest in
      match blood with
      | "A" -> (a + 1, b, o, ab)
      | "B" -> (a, b + 1, o, ab)
      | "O" -> (a, b, o + 1, ab)
      | "AB" -> (a, b, o, ab + 1)
      | _ -> (a, b, o, ab) )

(* テスト *)
let test_ketsueki_shukei1 = ketsueki_shukei [] = (0, 0, 0, 0)

let test_ketsueki_shukei2 = ketsueki_shukei [p1] = (1, 0, 0, 0)

let test_ketsueki_shukei3 = ketsueki_shukei [p2; p3; p4] = (0, 1, 1, 1)

let test_ketsueki_shukei4 = ketsueki_shukei [p1; p2; p3; p4] = (1, 1, 1, 1)

let test_ketsueki_shukei5 =
  ketsueki_shukei [p4; p1; p2; p3; p1] = (2, 1, 1, 1)

(* exer10.8 *)
let rec saita_ketsueki_list lst =
  match lst with
  | [] -> ("X", min_int)
  | ((b, sum) as ret) :: rest ->
      let ((mb, msum) as m) = saita_ketsueki_list rest in
      if sum >= msum then
        ret
      else
        m

(* 目的：person_t 型のリストを受け取ったら，4つの血液型のうち，最も人数の多かった血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let rec saita_ketsueki lst =
  let a, b, o, ab = ketsueki_shukei lst in
  fst (saita_ketsueki_list [("A", a); ("B", b); ("O", o); ("AB", ab)])

(* テスト *)
let test_saita_ketsueki1 = saita_ketsueki [] = "A" (* 全て0ならA *)

let test_saita_ketsueki2 = saita_ketsueki [p1] = "A"

let test_saita_ketsueki3 = saita_ketsueki [p2; p3] = "B" (* BとOが同数ならB *)

let test_saita_ketsueki4 = saita_ketsueki [p2; p2; p3] = "B"

let test_saita_ketsueki5 = saita_ketsueki [p1; p1; p2; p3; p4] = "A"

let rec maximum lst =
  match lst with
  | [] -> min_int
  | first :: rest ->
      let max_rest = maximum rest in
      if first >= max_rest then
        first
      else
        max_rest

let rec saita_ketsueki_all lst max_n =
  match lst with
  | [] -> []
  | (b, sum) :: rest ->
      let rest_result = saita_ketsueki_all rest max_n in
      if sum = max_n then
        b :: rest_result
      else
        rest_result

let saita_ketsueki' lst =
  let a, b, o, ab = ketsueki_shukei lst in
  let max_n = maximum [a; b; o; ab] in
  if a + b + o + ab = 0 then
    []
  else
    saita_ketsueki_all [("A", a); ("B", b); ("O", o); ("AB", ab)] max_n

let test_saita_ketsueki1' = saita_ketsueki' [] = []

let test_saita_ketsueki2' = saita_ketsueki' [p1] = ["A"]

let test_saita_ketsueki3' = saita_ketsueki' [p2; p3] = ["B"; "O"]
(* BとOが同数ならB *)

let test_saita_ketsueki4' = saita_ketsueki' [p2; p2; p3] = ["B"]

let test_saita_ketsueki5' = saita_ketsueki' [p1; p1; p2; p3; p4] = ["A"]
