(* exer 7.1 *)

(* 目的：国語，数学，英語，理科，社会の 5 教科の点数を与えられたら，その合計と平均を組みにして返す *)
(* goukei_to_heikin : int -> int -> int -> int -> int -> (int, int) *)
let goukei_to_heikin kokugo sugaku eigo rika syakai =
  (kokugo + sugaku + eigo + rika + syakai, (kokugo + sugaku + eigo + rika + syakai) / 5)

(* テスト *)
(* 100, 100, 100, 100, 100 → (500, 100) *)
let goukei_to_heikin_test1 =
  goukei_to_heikin 100 100 100 100 100 = (500, 100)

(* 70, 80, 90, 60, 50 → (350, 70) *)
let goukei_to_heikin_test2 =
  goukei_to_heikin 70 80 90 60 50 = (350, 70)

(* 10, 20, 30, 40, 50 → (150, 30) *)
let goukei_to_heikin_test3 =
  goukei_to_heikin 10 20 30 40 50 = (150, 30)

(* 100, 0, 0, 0, 0 → (100, 20) *)
let goukei_to_heikin_test4 =
  goukei_to_heikin 100 0 0 0 0 = (100, 20)

(* 0, 0, 0, 0, 0 → (0, 0) *)
let goukei_to_heikin_test5 =
  goukei_to_heikin 0 0 0 0 0 = (0, 0)


(* exer 7.2 *)

(* 目的：名前と成績の組を受け取り，「○○さんの評価は△です」という文字列を返す *)
(* seiseki : string * string -> string *)
let seiseki (name, result) = name ^ "さんの評価は" ^ result ^ "です"

(* テスト *)
(* ("田中", "A") → "田中さんの評価はAです" *)
let seiseki_test1 =
  seiseki ("田中", "A") = "田中さんの評価はAです"

(* ("山田", "B") → "山田さんの評価はBです" *)
let seiseki_test2 =
  seiseki ("山田", "B") = "山田さんの評価はBです"

(* ("佐藤", "C") → "佐藤さんの評価はCです" *)
let seiseki_test3 =
  seiseki ("佐藤", "C") = "佐藤さんの評価はCです"

(* ("Alice", "S") → "Aliceさんの評価はSです" *)
let seiseki_test4 =
  seiseki ("Alice", "S") = "Aliceさんの評価はSです"

(* ("Bob", "F") → "Bobさんの評価はFです" *)
let seiseki_test5 =
  seiseki ("Bob", "F") = "Bobさんの評価はFです"

(* exer 7.3 *)

(* 目的：x 座標と y 座標の組を受け取り，x 軸について対称な点の座標を返す *)
(* taisho_x : float * float -> float * float *)
let taisho_x pair = match pair with
  (x, y) -> (x, -. y)

(* テスト *)
(* (3.0, 4.0) → (3.0, -4.0) *)
let taisho_x_test1 =
  taisho_x (3.0, 4.0) = (3.0, -.4.0)

(* (-2.5, 1.0) → (-2.5, -1.0) *)
let taisho_x_test2 =
  taisho_x (-2.5, 1.0) = (-2.5, -.1.0)

(* (0.0, 5.0) → (0.0, -5.0) *)
let taisho_x_test3 =
  taisho_x (0.0, 5.0) = (0.0, -.5.0)

(* (1.2, -3.4) → (1.2, 3.4) *)
let taisho_x_test4 =
  taisho_x (1.2, -.3.4) = (1.2, 3.4)

(* y = 0 の場合は変化なし *)
let taisho_x_test5 =
  taisho_x (7.7, 0.0) = (7.7, 0.0)

(* exer 7.4 *)

(* 目的：x 座標と y 座標の組を二つ受け取り，その中点の座標を返す *)
(* chuten : float * float -> float * float *)
let chuten pair1 pair2 = match (pair1, pair2) with
  ((x1, y1), (x2, y2)) -> ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0)

(* テスト *)
(* (0.0, 0.0) と (2.0, 2.0) → (1.0, 1.0) *)
let chuten_test1 =
  chuten (0.0, 0.0) (2.0, 2.0) = (1.0, 1.0)

(* (-1.0, 4.0) と (3.0, -2.0) → (1.0, 1.0) *)
let chuten_test2 =
  chuten (-1.0, 4.0) (3.0, -.2.0) = (1.0, 1.0)

(* (5.0, 5.0) と (5.0, -5.0) → (5.0, 0.0) *)
let chuten_test3 =
  chuten (5.0, 5.0) (5.0, -.5.0) = (5.0, 0.0)

(* (1.2, 3.4) と (5.6, 7.8) → (3.4, 5.6) *)
let chuten_test4 =
  chuten (1.2, 3.4) (5.6, 7.8) = (3.4, 5.6)

(* 同じ点同士 → 同じ点 *)
let chuten_test5 =
  chuten (2.5, 3.5) (2.5, 3.5) = (2.5, 3.5)
