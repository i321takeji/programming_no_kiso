(* exer 5.1 *)

(* 目的：受け取った実数 x の絶対値を計算する *)
(* jikan : int -> string *)
let jikan h =
  if 0 <= h &&  h <= 12 then "午前"
  else if 12 < h && h <= 23 then "午後"
  else "エラー"

(* テスト *)
let jikan_test1 = jikan 0 = "午前"
let jikan_test2 = jikan 12 = "午前"
let jikan_test3 = jikan 13 = "午後"


(* exer 5.2 *)

(* 目的：誕生日 (月と日) を受け取ったら，星座を返す *)
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

let seiza_test1 = seiza 3 21 = "牡羊座"
let seiza_test2 = seiza 4 19 = "牡羊座"
let seiza_test3 = seiza 4 20 = "牡牛座"
let seiza_test4 = seiza 5 20 = "牡牛座"
let seiza_test5 = seiza 5 21 = "双子座"
let seiza_test6 = seiza 6 21 = "双子座"
let seiza_test7 = seiza 6 22 = "蟹座"
let seiza_test8 = seiza 7 22 = "蟹座"
let seiza_test9 = seiza 7 23 = "獅子座"
let seiza_test10 = seiza 8 22 = "獅子座"
let seiza_test11 = seiza 8 23 = "乙女座"
let seiza_test12 = seiza 9 22 = "乙女座"
let seiza_test13 = seiza 9 23 = "天秤座"
let seiza_test14 = seiza 10 23 = "天秤座"
let seiza_test15 = seiza 10 24 = "蠍座"
let seiza_test16 = seiza 11 22 = "蠍座"
let seiza_test17 = seiza 11 23 = "射手座"
let seiza_test18 = seiza 12 21 = "射手座"
let seiza_test19 = seiza 12 22 = "山羊座"
let seiza_test20 = seiza 1 19 = "山羊座"
let seiza_test21 = seiza 1 20 = "水瓶座"
let seiza_test22 = seiza 2 18 = "水瓶座"
let seiza_test23 = seiza 2 19 = "魚座"
let seiza_test24 = seiza 3 20 = "魚座"

(*
星座    誕生日の範囲
牡羊座（おひつじ座）    3月21日 ～ 4月19日
牡牛座（おうし座）    4月20日 ～ 5月20日
双子座（ふたご座）    5月21日 ～ 6月21日
蟹座（かに座）    6月22日 ～ 7月22日
獅子座（しし座）    7月23日 ～ 8月22日
乙女座（おとめ座）    8月23日 ～ 9月22日
天秤座（てんびん座）    9月23日 ～ 10月23日
蠍座（さそり座）    10月24日 ～ 11月22日
射手座（いて座）    11月23日 ～ 12月21日
山羊座（やぎ座）    12月22日 ～ 1月19日
水瓶座（みずがめ座）    1月20日 ～ 2月18日
魚座（うお座）    2月19日 ～ 3月20日
*)