(* exer 9.1 *)
let ex91 = "春" :: "夏" :: "秋" :: "冬" :: []

(* exer 9.2 *)
(* 人と名前，身長 (m)，体重 (kg)，誕生日 (月と日)，血液型を表す型 *)
type person_t = {
  name : string;
  height : float;
  weight : float;
  birth : (int * int);
  blood : string;
}

(* let ex1person = { name = "taro"; height = 1.7; weight = 65.0; birth = (2, 10); blood = "A" } *)
(* let ex2person = { name = "jaro"; height = 1.6; weight = 75.3; birth = (3, 9); blood = "B" } *)
(* let ex3person = { name = "saburo"; height = 1.8; weight = 55.1; birth = (10, 23); blood = "O" } *)

let ex92 = { name = "taro"; height = 1.7; weight = 65.0; birth = (2, 10); blood = "A" } ::
           { name = "jaro"; height = 1.6; weight = 75.3; birth = (3, 9); blood = "B" } ::
           { name = "saburo"; height = 1.8; weight = 55.1; birth = (10, 23); blood = "O" } :: []

(* exer 9.3 *)
let ex931 = ["春"; "夏"; "秋"; "冬"]
let ex932 = [{ name = "taro"; height = 1.7; weight = 65.0; birth = (2, 10); blood = "A" };
             { name = "jaro"; height = 1.6; weight = 75.3; birth = (3, 9); blood = "B" };
             { name = "saburo"; height = 1.8; weight = 55.1; birth = (10, 23); blood = "O" }]

(* int list は
     - []              空リスト，あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
   という形 *)

(* 目的：受け取ったリスト lst に 0 が含まれているかを調べる *)
(* contain_zero : int list -> bool *)
let rec contain_zero lst = match lst with
    [] -> false
  | first :: rest -> if first = 0 then true
                                  else contain_zero rest

(* テスト *)
let test1 = contain_zero [] = false
let test2 = contain_zero [0; 2] = true
let test3 = contain_zero [1; 2] = false
let test4 = contain_zero [1; 2; 3; 0; 5; 6; 7] = true
let test5 = contain_zero [1; 2; 3; 4; 5; 6; 7] = false

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest

(* テスト *)
let test1_sum = sum [] = 0
let test2_sum = sum [2] = 2
let test3_sum = sum [1; 3] = 4
let test4_sum = sum [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] = 55

(* exer9.4 *)
(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let rec length lst = match lst with
    [] -> 0
  | _ :: rest -> 1 + length rest

let test1_length = length [] = 0
let test2_length = length [2] = 1
let test3_length = length [1; 3] = 2
let test4_length = length [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] = 10

(* exer9.5 *)
(* 目的：受け取ったリスト lst から偶数の要素のみ含むリストを求める *)
(* even : int list -> int list *)
let rec even lst = match lst with
    [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest
                                          else even rest

let test_even1 = even [] = []
let test_even2 = even [1; 3; 5] = []
let test_even3 = even [2; 4; 6] = [2; 4; 6]
let test_even4 = even [1; 2; 3; 4; 5] = [2; 4]
let test_even5 = even [-2; -1; 0; 7; 8] = [-2; 0; 8]

(* exer9.6 *)
(* 目的：文字列のリスト lst を受け取ったら，それらを連結した結果を返す *)
(* concat : string list -> string *)
let rec concat lst = match lst with
    [] -> ""
  | first :: rest ->  first ^ concat rest

let test_concat1 = concat [] = ""
let test_concat2 = concat ["a"] = "a"
let test_concat3 = concat ["ab";"cd"] = "abcd"
let test_concat4 = concat ["春";"夏";"秋";"冬"] = "春夏秋冬"
let test_concat5 = concat ["a";"";"b";""] = "ab"

(* section 9.6 *)
type gakusei_t = {
  namae : string;   (* 名前 *)
  tensuu : int;     (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* gakusei_t list 型のデータの例 *)
let lst1 = []
let lst2 = [{ namae = "asai"; tensuu = 70; seiseki = "B "}]
let lst3 = [{ namae = "asai"; tensuu = 70; seiseki = "B"};
            { namae = "kaneko"; tensuu = 85; seiseki = "A"}]
let lst4 = [{ namae = "yoshida"; tensuu = 80; seiseki = "A"};
            { namae = "asai"; tensuu = 70; seiseki = "B"};
            { namae = "kaneko"; tensuu = 85; seiseki = "A"}]

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let rec count_A lst = match lst with
  [] -> 0
| { namae = n; tensuu = t; seiseki = s } :: rest
  -> if s = "A" then 1 + count_A rest
                else count_A rest
(* | first :: rest -> (match first with
                      { namae = n; tensuu = t; seiseki = s }
                        -> 0) *)

let test_count_A = count_A lst1 = 0
let test_count_A = count_A lst2 = 0
let test_count_A = count_A lst3 = 1
let test_count_A = count_A lst4 = 2

(* exer9.7 *)
type person_t = {
  name : string;
  height : float;
  weight : float;
  birth : (int * int);
  blood : string;
}

let ex1person = { name = "taro"; height = 1.7; weight = 65.0; birth = (2, 10); blood = "A" }
let ex2person = { name = "jaro"; height = 1.6; weight = 75.3; birth = (3, 9); blood = "B" }
let ex3person = { name = "saburo"; height = 1.8; weight = 55.1; birth = (10, 23); blood = "O" }

(* 目的：人のリスト lst のうち，血液型が A の人の数を返す *)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A lst = match lst with
    [] -> 0
  | { name = n; height = h; weight = w; birth = b; blood = bt } :: rest
    -> if (bt = "A") then 1 + count_ketsueki_A rest
                     else count_ketsueki_A rest

let rec count_ketsueki_A' lst = match lst with
    [] -> 0
  | { blood } :: rest
    -> if (blood = "A") then 1 + count_ketsueki_A' rest
                        else count_ketsueki_A' rest

(* テスト用データ *)
let p1 = { name = "taro"; height = 1.7; weight = 60.0; birth = (1,1); blood = "A" }
let p2 = { name = "jiro"; height = 1.8; weight  = 70.0; birth = (2,2); blood = "B" }
let p3 = { name = "hanako"; height = 1.6; weight  = 50.0; birth = (3,3); blood = "O" }
let p4 = { name = "yuki"; height = 1.5; weight  = 48.0; birth = (4,4); blood = "A" }

(* テスト *)
let test_ketsueki1 = count_ketsueki_A [] = 0
let test_ketsueki2 = count_ketsueki_A [p1] = 1
let test_ketsueki3 = count_ketsueki_A [p2;p3] = 0
let test_ketsueki4 = count_ketsueki_A [p1;p2;p3] = 1
let test_ketsueki5 = count_ketsueki_A [p1;p4;p2;p3] = 2

(* exer9.8*)

(* seiza : int -> int -> string *)
let seiza m d =
  if m < 1 || m > 12 || d < 1 || d > 31 then "エラー"
  else if m = 1 then
    (if d <= 19 then "山羊座"
     else if d <= 31 then "水瓶座"
     else "エラー")
  else if m = 2 then
    (if d <= 18 then "水瓶座"
     else if d <= 28 then "魚座"
     else "エラー")
  else if m = 3 then
    (if d <= 20 then "魚座"
     else if d <= 31 then "牡羊座"
     else "エラー")
  else if m = 4 then
    (if d <= 19 then "牡羊座"
     else if d <= 30 then "牡牛座"
     else "エラー")
  else if m = 5 then
    (if d <= 20 then "牡牛座"
     else if d <= 31 then "双子座"
     else "エラー")
  else if m = 6 then
    (if d <= 21 then "双子座"
     else if d <= 30 then "蟹座"
     else "エラー")
  else if m = 7 then
    (if d <= 22 then "蟹座"
     else if d <= 31 then "獅子座"
     else "エラー")
  else if m = 8 then
    (if d <= 22 then "獅子座"
     else if d <= 31 then "乙女座"
     else "エラー")
  else if m = 9 then
    (if d <= 22 then "乙女座"
     else if d <= 30 then "天秤座"
     else "エラー")
  else if m = 10 then
    (if d <= 23 then "天秤座"
     else if d <= 31 then "蠍座"
     else "エラー")
  else if m = 11 then
    (if d <= 22 then "蠍座"
     else if d <= 30 then "射手座"
     else "エラー")
  else if m = 12 then
    (if d <= 21 then "射手座"
     else if d <= 31 then "山羊座"
     else "エラー")
  else "エラー"

(* 目的：person_t 型のデータのリストを受け取り，乙女座の人の名前からなるリストを返す *)
(* otomeza : person_t list -> string list *)
let rec otomeza lst = match lst with
    [] -> []
  | { name = n; height = h; weight = w; birth = (m, d); blood = bt } :: rest
    -> if seiza m d = "乙女座" then n :: otomeza rest
                               else otomeza rest

let rec otomeza' lst = match lst with
    [] -> []
  | { name; birth = (m, d) } :: rest
    -> if seiza m d = "乙女座" then name :: otomeza' rest
                               else otomeza' rest

(* テスト用データ *)
let p_virgo1 = { name = "miho"; height = 1.6; weight = 50.0; birth = (8,23); blood = "A" }
let p_virgo2 = { name = "sota"; height = 1.7; weight = 60.0; birth = (9,22); blood = "O" }
let p_not_virgo1 = { name = "ryo"; height = 1.75; weight = 68.0; birth = (8,22); blood = "B" }
let p_not_virgo2 = { name = "kana"; height = 1.58; weight = 48.0; birth = (9,23); blood = "AB" }

(* テスト *)
let test_otomeza1 = otomeza [] = []
let test_otomeza2 = otomeza [p_not_virgo1; p_not_virgo2] = []
let test_otomeza3 = otomeza [p_virgo1] = ["miho"]
let test_otomeza4 = otomeza [p_virgo1; p_not_virgo1; p_not_virgo2] = ["miho"]
let test_otomeza5 = otomeza [p_not_virgo1; p_virgo1; p_virgo2; p_not_virgo2] = ["miho"; "sota"]
