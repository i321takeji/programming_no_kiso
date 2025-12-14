(* exer 5.2 *)

(* 目的：受け取った実数 x の絶対値を計算する *)
(* jikan : int -> string *)
let jikan h =
  if 0 <= h && h <= 12 then "午前"
  else if 12 < h && h <= 23 then "午後"
  else "エラー"

(* テスト *)
let jikan_test1 = jikan 0 = "午前"

let jikan_test2 = jikan 12 = "午前"

let jikan_test3 = jikan 13 = "午後"

(* exer 5.3 *)

(* 目的：誕生日 (月と日) を受け取ったら，星座を返す *)
(* seiza : int -> int -> string *)
let seiza m d =
  if m < 1 || m > 12 || d < 1 || d > 31 then "エラー"
  else if m = 1 then
    if d <= 19 then "山羊座" else if d <= 31 then "水瓶座" else "エラー"
  else if m = 2 then
    if d <= 18 then "水瓶座" else if d <= 28 then "魚座" else "エラー"
  else if m = 3 then
    if d <= 20 then "魚座" else if d <= 31 then "牡羊座" else "エラー"
  else if m = 4 then
    if d <= 19 then "牡羊座" else if d <= 30 then "牡牛座" else "エラー"
  else if m = 5 then
    if d <= 20 then "牡牛座" else if d <= 31 then "双子座" else "エラー"
  else if m = 6 then
    if d <= 21 then "双子座" else if d <= 30 then "蟹座" else "エラー"
  else if m = 7 then
    if d <= 22 then "蟹座" else if d <= 31 then "獅子座" else "エラー"
  else if m = 8 then
    if d <= 22 then "獅子座" else if d <= 31 then "乙女座" else "エラー"
  else if m = 9 then
    if d <= 22 then "乙女座" else if d <= 30 then "天秤座" else "エラー"
  else if m = 10 then
    if d <= 23 then "天秤座" else if d <= 31 then "蠍座" else "エラー"
  else if m = 11 then
    if d <= 22 then "蠍座" else if d <= 30 then "射手座" else "エラー"
  else if m = 12 then
    if d <= 21 then "射手座" else if d <= 31 then "山羊座" else "エラー"
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

(* 星座 誕生日の範囲 牡羊座（おひつじ座） 3月21日 ～ 4月19日 牡牛座（おうし座） 4月20日 ～ 5月20日 双子座（ふたご座） 5月21日
   ～ 6月21日 蟹座（かに座） 6月22日 ～ 7月22日 獅子座（しし座） 7月23日 ～ 8月22日 乙女座（おとめ座） 8月23日 ～
   9月22日 天秤座（てんびん座） 9月23日 ～ 10月23日 蠍座（さそり座） 10月24日 ～ 11月22日 射手座（いて座） 11月23日 ～
   12月21日 山羊座（やぎ座） 12月22日 ～ 1月19日 水瓶座（みずがめ座） 1月20日 ～ 2月18日 魚座（うお座） 2月19日 ～
   3月20日 *)

(* exer 5.4 *)

(* 目的：実数 a, b, c を与えたら，2次方程式 ax^2 + bx + c = 0 の判別式の値を返す *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = (b ** 2.0) -. (4.0 *. a *. c)

(* 2実根: D > 0 *)
let hanbetsushiki_test1 = hanbetsushiki 1.0 5.0 6.0 = 1.0 (* 25 - 24 = 1 *)

let hanbetsushiki_test2 = hanbetsushiki 2.0 4.0 1.0 = 8.0 (* 16 - 8 = 8 *)

(* 重根: D = 0 *)
let hanbetsushiki_test3 = hanbetsushiki 1.0 2.0 1.0 = 0.0 (* 4 - 4 = 0 *)

let hanbetsushiki_test4 = hanbetsushiki 3.0 6.0 3.0 = 0.0 (* 36 - 36 = 0 *)

(* 虚数解: D < 0 *)
let hanbetsushiki_test5 = hanbetsushiki 1.0 2.0 3.0 = -8.0 (* 4 - 12 = -8 *)

let hanbetsushiki_test6 =
  hanbetsushiki 2.0 1.0 2.0 = -15.0 (* 1 - 16 = -15 *)

(* 特殊ケース *)
let hanbetsushiki_test7 =
  hanbetsushiki 0.0 2.0 1.0 = 4.0 (* a=0: 4 - 0 = 4 *)

let hanbetsushiki_test8 =
  hanbetsushiki 1.0 0.0 (-1.0) = 4.0 (* 0 - (-4) = 4 *)

let hanbetsushiki_test9 = hanbetsushiki 1.0 0.0 1.0 = -4.0 (* 0 - 4 = -4 *)

(* exer 5.5 *)

(* 目的：実数 a, b, c を与えたら，2次方程式 ax^2 + bx + c = 0 の解の個数を返す
   実数解 1, 重解 0, 虚数解 -1 *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  let hanbetsu = hanbetsushiki a b c in
  if hanbetsu > 0.0 then 1 else if hanbetsu < 0.0 then -1 else 0

(* D > 0 のケース（2実根） *)
let kai_no_kosuu_test1 = kai_no_kosuu 1.0 5.0 6.0 = 1 (* D = 25 - 24 = 1 *)

let kai_no_kosuu_test2 = kai_no_kosuu 2.0 4.0 1.0 = 1 (* D = 16 - 8 = 8 *)

(* D = 0 のケース（重根） *)
let kai_no_kosuu_test3 = kai_no_kosuu 1.0 2.0 1.0 = 0 (* D = 4 - 4 = 0 *)

let kai_no_kosuu_test4 = kai_no_kosuu 3.0 6.0 3.0 = 0 (* D = 36 - 36 = 0 *)

(* D < 0 のケース（虚数解） *)
let kai_no_kosuu_test5 = kai_no_kosuu 1.0 2.0 3.0 = -1 (* D = 4 - 12 = -8 *)

let kai_no_kosuu_test6 = kai_no_kosuu 2.0 1.0 2.0 = -1 (* D = 1 - 16 = -15 *)

(* 追加の境界・特殊値テスト *)
let kai_no_kosuu_test7 =
  kai_no_kosuu 0.0 2.0 1.0 = 1 (* D = 4 - 0 = 4 → 一次方程式的扱い *)

let kai_no_kosuu_test8 =
  kai_no_kosuu 1.0 0.0 (-1.0) = 1 (* D = 0 - (-4) = 4 → 正 *)

let kai_no_kosuu_test9 =
  kai_no_kosuu 1.0 0.0 1.0 = -1 (* D = 0 - 4 = -4 → 負 *)

(* exer 5.6 *)

(* 目的：実数 a, b, c を与えたら，2次方程式 ax^2 + bx + c = 0 の解が虚数解を持つか判定 *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosuukai a b c = kai_no_kosuu a b c = -1

(* D > 0 → 実数解 → false *)
let kyosuukai_test1 = kyosuukai 1.0 5.0 6.0 = false (* D = 1.0 *)

let kyosuukai_test2 = kyosuukai 2.0 4.0 1.0 = false (* D = 8.0 *)

(* D = 0 → 重根（実数） → false *)
let kyosuukai_test3 = kyosuukai 1.0 2.0 1.0 = false (* D = 0.0 *)

let kyosuukai_test4 = kyosuukai 3.0 6.0 3.0 = false (* D = 0.0 *)

(* D < 0 → 虚数解 → true *)
let kyosuukai_test5 = kyosuukai 1.0 2.0 3.0 = true (* D = -8.0 *)

let kyosuukai_test6 = kyosuukai 2.0 1.0 2.0 = true (* D = -15.0 *)

(* 特殊ケース（a=0 の一次方程式扱い）→ D = b² → false *)
let kyosuukai_test7 = kyosuukai 0.0 2.0 1.0 = false (* D = 4.0 *)

let kyosuukai_test8 = kyosuukai 1.0 0.0 1.0 = true (* D = -4.0 → 虚数解 *)

let kyosuukai_test9 = kyosuukai 1.0 0.0 (-1.0) = false (* D = 4.0 → 実数解 *)

(* exer 5.7 *)

(* from exer 4.4*)
let bmi h w = w /. (h ** 2.0)

(* 目的：身長 (m) と体重 (kg) を与えたら，BMI 指数を計算し，その数値によって体系を返す *)
(* taikei : float -> float -> string *)
let taikei h w =
  if bmi h w < 18.5 then "やせ"
  else if bmi h w < 25.0 then "標準"
  else if bmi h w < 30.0 then "肥満"
  else "高度肥満"

(* やせ: BMI < 18.5 *)
let taikei_test1 = taikei 1.70 50.0 = "やせ" (* BMI ≒ 17.3 *)

let taikei_test2 = taikei 1.60 45.0 = "やせ" (* BMI ≒ 17.6 *)

(* 標準: 18.5 ≤ BMI < 25 *)
let taikei_test3 = taikei 1.70 65.0 = "標準" (* BMI ≒ 22.5 *)

let taikei_test4 = taikei 1.60 60.0 = "標準" (* BMI ≒ 23.4 *)

(* 肥満: 25 ≤ BMI < 30 *)
let taikei_test5 = taikei 1.70 75.0 = "肥満" (* BMI ≒ 25.95 *)

let taikei_test6 = taikei 1.60 75.0 = "肥満" (* BMI ≒ 29.3 *)

(* 高度肥満: BMI ≥ 30 *)
let taikei_test7 = taikei 1.70 90.0 = "高度肥満" (* BMI ≒ 31.1 *)

let taikei_test8 = taikei 1.50 75.0 = "高度肥満" (* BMI ≒ 33.3 *)

(* 境界値チェック *)
let taikei_test9 = taikei 1.70 53.465 = "標準" (* BMI ≒ 18.5ぴったり → 標準 *)

let taikei_test10 = taikei 1.70 72.25 = "肥満" (* BMI ≒ 25.0ぴったり → 肥満 *)

let taikei_test11 = taikei 1.70 86.7 = "高度肥満" (* BMI ≒ 30.0ぴったり → 高度肥満 *)
