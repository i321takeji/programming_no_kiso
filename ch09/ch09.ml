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
let contain_zero lst = false

(* テスト *)
let test1 = contain_zero [] = false
let test2 = contain_zero [0; 2] = true
let test3 = contain_zero [1; 2] = false
let test4 = contain_zero [1; 2; 3; 0; 5; 6; 7] = true
let test5 = contain_zero [1; 2; 3; 4; 5; 6; 7] = false
