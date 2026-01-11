(* 目的 *)
(* fac : int -> int *)
let rec fac n =
  if n = 0 then
    1
  else
    n * fac (n - 1)

(* テスト *)
let test1 = fac 0 = 1

let test2 = fac 1 = 1

let test3 = fac 2 = 2

let test4 = fac 3 = 6

let test5 = fac 4 = 24

let test6 = fac 10 = 362880

(* 目的：自然数 m と n を受け取ったら m の n 乗を求める *)
(* power : int -> int -> int *)
let rec power m n =
  if n = 0 then
    1
  else
    m * power m (n - 1)

(* テスト *)
let test1 = power 3 0 = 1

let test2 = power 3 1 = 3

let test3 = power 3 2 = 9

let test4 = power 3 3 = 27

(* exer 11.1 *)
(* 目的：0 から受け取った自然数までの 2 乗の和を求める *)
(* sum_of_square : int -> int *)
let rec sum_of_square n =
  if n = 0 then
    0
  else
    (n * n) + sum_of_square (n - 1)

(* テスト *)
let test_sum_of_square1 = sum_of_square 0 = 0

let test_sum_of_square2 = sum_of_square 1 = 1

let test_sum_of_square3 = sum_of_square 2 = 5

let test_sum_of_square4 = sum_of_square 3 = 14

let test_sum_of_square5 = sum_of_square 4 = 30

let test_sum_of_square6 = sum_of_square 5 = 55

(* exer 11.2 *)
(* 目的： 漸化式 a_n の第 n 項を求める *)
(* a : int -> int *)
let rec a n =
  if n = 0 then
    3
  else
    (2 * a (n - 1)) - 1

(* テスト *)
let test_a1 = a 0 = 3

let test_a2 = a 1 = 5

let test_a3 = a 2 = 9

let test_a4 = a 3 = 17

let test_a5 = a 4 = 33
