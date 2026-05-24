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
