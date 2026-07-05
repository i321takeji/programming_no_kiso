(* sec8.1 *)

(* 八百屋においてある野菜と値段のリストの例 *)
let yaoya_list = [("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200)]

(* 目的：item の値段を調べる *)
(* price : string -> (string * int) list -> int option *)
let rec price item yaoya_list =
  match yaoya_list with
  | [] -> None
  | (yasai, nedan) :: rest ->
      if item = yasai then
        Some nedan
      else
        price item rest

(* exer18.1 *)
(* 目的：問題 8.3 で定義した person_t 型のリストを受け取ったら，
   その中から最初の A 型の人のレコードをオプション型で返す関数．
   A 型の人がいなかったら None を返す *)

(* 人と名前，身長 (m)，体重 (kg)，誕生日 (月と日)，血液型を表す型 *)
type person_t =
  { name : string
  ; height : float
  ; weight : float
  ; birth : int * int
  ; blood : string
  }

(* first_A : person_t list -> person_t option *)
let rec first_A person_lst =
  match person_lst with
  | [] -> None
  | first :: rest ->
      if first.blood = "A" then
        Some first
      else
        first_A rest

(* テスト用データ *)
let p1 =
  { name = "taro"; height = 1.7; weight = 60.0; birth = (1, 1); blood = "B" }

let p2 =
  { name = "jiro"; height = 1.8; weight = 70.0; birth = (2, 2); blood = "A" }

let p3 =
  { name = "hanako"
  ; height = 1.6
  ; weight = 50.0
  ; birth = (3, 3)
  ; blood = "O"
  }

let p4 =
  { name = "yuki"; height = 1.5; weight = 48.0; birth = (4, 4); blood = "A" }

(* テスト *)
let test1_first_A = first_A [] = None

let test2_first_A = first_A [p1; p3] = None

let test3_first_A = first_A [p2] = Some p2

let test4_first_A = first_A [p1; p2; p3] = Some p2

let test5_first_A = first_A [p1; p2; p4] = Some p2

(* sec8.2 *)

(* 目的：yasai_list を買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int *)
let rec total_price yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | first :: rest -> (
    match price first yaoya_list with
    | None -> total_price rest yaoya_list
    | Some p -> p + total_price rest yaoya_list )

(* exer18.2 *)
(* 目的：野菜のリストと八百屋のリストを受け取ったら，
   野菜のリストのうち八百屋にはおいてない野菜の数を返す関数 *)
(* count_urikire_yasai : string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | first :: rest -> (
    match price first yaoya_list with
    | None -> 1 + count_urikire_yasai rest yaoya_list
    | Some _ -> count_urikire_yasai rest yaoya_list )

let rec count_urikire_yasai yasai_list yaoya_list =
  List.fold_right
    (fun yasai acc ->
      match price yasai yaoya_list with
      | None -> 1 + acc
      | Some _ -> acc )
    yasai_list 0

(* テスト *)
let test1_count_urikire_yasai = count_urikire_yasai [] yaoya_list = 0

let test2_count_urikire_yasai =
  count_urikire_yasai ["トマト"; "たまねぎ"] yaoya_list = 0

let test3_count_urikire_yasai =
  count_urikire_yasai ["トマト"; "キャベツ"] yaoya_list = 1

let test4_count_urikire_yasai =
  count_urikire_yasai ["キャベツ"; "レタス"; "トマト"] yaoya_list = 2

let test5_count_urikire_yasai = count_urikire_yasai ["トマト"; "キャベツ"] [] = 2

(* sec8.3 *)

(* 目的：yasai_list を買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int option *)
let rec total_price yasai_list yaoya_list =
  match yasai_list with
  | [] -> Some 0
  | first :: rest -> (
    match price first yaoya_list with
    | None -> None
    | Some p -> (
      match total_price rest yaoya_list with
      | None -> None
      | Some q -> Some (p + q) ) )

(* 目的：yasai_list を買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int *)
let rec total_price yasai_list yaoya_list =
  (* 目的：yasai_list を買ったときの値段の合計を調べる *)
  (* hojo : string list -> int option *)
  let rec hojo yasai_list =
    match yasai_list with
    | [] -> Some 0
    | first :: rest -> (
      match price first yaoya_list with
      | None -> None
      | Some p -> (
        match hojo rest with
        | None -> None
        | Some q -> Some (p + q) ) )
  in
  match hojo yasai_list with
  | None -> 0
  | Some p -> p

(* sec18.5 *)

(* 売り切れを示す例外 *)
exception Urikire

(* 目的：item の値段を調べる *)
(* 見つからないときには Urikire という例外を発生する *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_list =
  match yaoya_list with
  | [] -> raise Urikire
  | (yasai, nedan) :: rest ->
      if item = yasai then
        nedan
      else
        price item rest

(* 目的：yasai_list を買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int *)
let rec total_price yasai_list yaoya_list =
  (* 目的：yasai_list を買ったときの値段の合計を調べる *)
  (* hojo : string list -> int *)
  let rec hojo yasai_list =
    match yasai_list with
    | [] -> 0
    | first :: rest -> price first yaoya_list + hojo rest
  in
  try hojo yasai_list with
  | Urikire -> 0

(* exer18.3 *)
(* 目的：問題 17.11 で作成した関数 assoc を，
   キーが見つからなかった場合には Not_found という例外を起こすように変更 *)
(* assoc : 'a -> ('a * 'b) list -> 'b *)
let rec assoc key assoc_lst =
  match assoc_lst with
  | [] -> raise Not_found
  | (k, v) :: rest ->
      if key = k then
        v
      else
        assoc key rest

(* テスト *)
let is_not_found f =
  try f () ; false with
  | Not_found -> true

let test1_assoc = is_not_found (fun () -> assoc "茗荷谷" [])

let test2_assoc = assoc "茗荷谷" [("茗荷谷", 1.2)] = 1.2

let test3_assoc =
  assoc "後楽園" [("茗荷谷", 1.2); ("後楽園", 1.8); ("本郷三丁目", 0.8)] = 1.8

let test4_assoc =
  is_not_found (fun () -> assoc "池袋" [("茗荷谷", 1.2); ("後楽園", 1.8)] )

(* sec18.6 *)

(* 目的：lst 中の整数すべて掛け合わせる *)
(* times : int list -> int *)
let rec times lst =
  match lst with
  | [] -> 1
  | first :: rest -> first * times rest

(* 0 が見つかったことを示す例外 *)
exception Zero

(* 目的：lst 中の整数すべて掛け合わせる *)
(* times : int list -> int *)
let times lst =
  (* 目的:lst 中の整数をすべて掛け合わせる *)
  (* 0 を見つけたら例外 Zero を起こす *)
  (* hojo : int list -> int *)
  let rec hojo lst =
    match lst with
    | [] -> 1
    | first :: rest ->
        if first = 0 then
          raise Zero
        else
          first * hojo rest
  in
  try hojo lst with
  | Zero -> 0
