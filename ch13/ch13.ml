type gakusei_t =
  { namae : string (* 名前 *)
  ; tensuu : int (* 点数 *)
  ; seiseki : string (* 成績 *)
  }

(* 目的：学生リスト lst のうち成績が seiseki0 の人の数を返す *)
(* count : gakusei_t list -> string -> int *)
let rec count lst seiseki0 =
  match lst with
  | [] -> 0
  | { namae = n; tensuu = t; seiseki = s } :: rest ->
      if s = seiseki0 then
        1 + count rest seiseki0
      else
        count rest seiseki0

let lst4 =
  [ { namae = "yoshida"; tensuu = 80; seiseki = "A" }
  ; { namae = "asai"; tensuu = 70; seiseki = "B" }
  ; { namae = "kaneko"; tensuu = 85; seiseki = "A" } ]

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst = count lst "A"

(* 目的：学生リスト lst のうち成績が B の人の数を返す *)
(* count_B : gakusei_t list -> int *)
let count_B lst = count lst "B"

(* exer13.1 *)
(* 人と名前，身長 (m)，体重 (kg)，誕生日 (月と日)，血液型を表す型 *)
type person_t =
  { name : string
  ; height : float
  ; weight : float
  ; birth : int * int
  ; blood : string
  }

(* 目的：person_t 型のリストを受け取ったら，その中から指定された血液型の人の数を返す関数 *)
(* count_ketsueki : person_t list -> string -> int *)
let rec count_ketsueki person_lst blood0 =
  match person_lst with
  | [] -> 0
  | { blood } :: rest ->
      if blood = blood0 then
        1 + count_ketsueki rest blood0
      else
        count_ketsueki rest blood0

(* テスト用データ *)
let p1 =
  { name = "a"; height = 160.0; weight = 50.0; birth = (1, 1); blood = "A" }

let p2 =
  { name = "b"; height = 170.0; weight = 60.0; birth = (2, 2); blood = "B" }

let p3 =
  { name = "c"; height = 180.0; weight = 70.0; birth = (3, 3); blood = "A" }

let p4 =
  { name = "d"; height = 150.0; weight = 45.0; birth = (4, 4); blood = "O" }

let p5 =
  { name = "e"; height = 165.0; weight = 55.0; birth = (5, 5); blood = "AB" }

let person_list1 = []

let person_list2 = [p1]

let person_list3 = [p1; p2; p3; p4; p5]

(* テスト *)
let test_count_ketsueki1 = count_ketsueki person_list1 "A" = 0

let test_count_ketsueki2 = count_ketsueki person_list2 "A" = 1

let test_count_ketsueki3 = count_ketsueki person_list2 "B" = 0

let test_count_ketsueki4 = count_ketsueki person_list3 "A" = 2

let test_count_ketsueki5 = count_ketsueki person_list3 "AB" = 1

(* 目的：実数のリスト lst を受け取り各要素の平方根のリストを返す *)
(* map_sqrt : flost list -> float list *)
let rec map_sqrt lst =
  match lst with
  | [] -> []
  | first :: rest -> sqrt first :: map_sqrt rest

(* 目的：学生のデータ gakusei を受け取り成績のついたデータを返す *)
(* hyouka : gakusei_t -> gakusei_t *)
let hyouka gakusei =
  match gakusei with
  | { namae = n; tensuu = t; seiseki = s } ->
      if t >= 80 then
        { namae = n; tensuu = t; seiseki = "A" }
      else if t >= 70 then
        { namae = n; tensuu = t; seiseki = "B" }
      else if t >= 60 then
        { namae = n; tensuu = t; seiseki = "C" }
      else
        { namae = n; tensuu = t; seiseki = "D" }

(* 目的：学生リスト lst を受け取り成績を入れたリストを返す *)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let rec map_hyouka lst =
  match lst with
  | [] -> []
  | first :: rest -> hyouka first :: map_hyouka rest

(* 目的：関数 f とリスト lst を受け取り f を施したリストを返す *)
(* map :: ('a -> 'b) -> 'a list -> 'b list *)
let rec map f lst =
  match lst with
  | [] -> []
  | first :: rest -> f first :: map f rest

(* 目的：実数のリスト lst を受け取り各要素の平方根のリストを返す *)
(* map_sqrt : flost list -> float list *)
let rec map_sqrt lst = map sqrt lst

(* 目的：学生リスト lst を受け取り成績を入れたリストを返す *)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let rec map_hyouka lst = map hyouka lst

(* exer13.2 *)
(* 目的：person_t 型のリストを受け取ったら，その中に出てくる人の名前のリストを返す *)
(* person_namae : person_t list -> string list *)
let rec person_namae person_lst =
  let get_name person = person.name in
  List.map get_name person_lst

(* テスト *)
(* テスト用データ *)
let person1 =
  { name = "o"; height = 100.0; weight = 80.0; birth = (10, 1); blood = "O" }

let person2 =
  { name = "a"; height = 200.0; weight = 50.0; birth = (2, 2); blood = "A" }

let person3 =
  { name = "c"; height = 150.0; weight = 70.0; birth = (4, 4); blood = "C" }

let test_person_namae1 = person_namae [] = []

let test_person_namae2 = person_namae [person1] = ["o"]

let test_person_namae3 = person_namae [person1; person2] = ["o"; "a"]

let test_person_namae4 =
  person_namae [person2; person3; person1] = ["a"; "c"; "o"]
