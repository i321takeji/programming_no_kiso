(* 目的：lst の中から n より p である要素のみを取り出す *)
(* take : int -> int list -> (int -> int -> bool) -> int list *)
let take n lst p = List.filter (fun item -> p item n) lst

(* 目的：lst の中から n より小さい要素のみを取り出す *)
(* take_less : int -> int list -> int list *)
let take_less n lst = take n lst ( <= )

(* 目的：lst の中から n より大きい要素のみを取り出す *)
(* take_greater : int -> int list -> int list *)
let take_greater n lst = take n lst ( > )

(* 目的：受け取った lst をクイックソートを使って昇順に整列する *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  match lst with
  | [] -> []
  | first :: rest ->
      quick_sort (take_less first rest)
      @ [first]
      @ quick_sort (take_greater first rest)

(* テスト *)
let test1 = quick_sort [] = []

let test2 = quick_sort [1] = [1]

let test3 = quick_sort [1; 2] = [1; 2]

let test4 = quick_sort [2; 1] = [1; 2]

let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]

(* 目的：級数の第 n 項の値を求める *)
(* dai_n_kou : int -> float *)
let rec dai_n_kou n =
  if n = 0 then
    1.0
  else
    dai_n_kou (n - 1) /. float_of_int n

(* 目的：e の近似を求める *)
(* e : int -> float *)
let rec e n =
  let d = dai_n_kou n in
  if d < 0.00001 then
    d
  else
    d +. e (n + 1)

(* exer15.2 *)
(* 目的：ふたつの自然数 m と n の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  if n = 0 then
    m
  else
    gcd n (m mod n)

(* テストケース *)
let test1_gcd = gcd 0 5 = 5

let test2_gcd = gcd 5 0 = 5

let test3_gcd = gcd 12 18 = 6

let test4_gcd = gcd 13 17 = 1

let test5_gcd = gcd 24 6 = 6

(* exer15.3 *)
(* 目的：2 <= n の自然数のリストを受け取ったら，2 <= n 以下の素数のリストを返す関数 *)
(* sieve : int list -> int list *)
let rec sieve lst =
  match lst with
  | [] -> []
  | first :: rest ->
      first :: sieve (List.filter (fun n -> n mod first <> 0) rest)

(* 目的：2 から n までの自然数のリスト *)
let from_2_to_n n =
  let rec f i =
    if i <= n then
      i :: f (i + 1)
    else
      []
  in
  f 2

(* 目的：自然数を受け取ったら，それ以下の素数のリストを返す関数 *)
let prime n = sieve (from_2_to_n n)

let test1_sieve = sieve [] = []

let test2_sieve = sieve [2] = [2]

let test3_sieve = sieve [2; 3; 4; 5; 6; 7; 8; 9; 10] = [2; 3; 5; 7]

let test4_sieve =
  sieve [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13] = [2; 3; 5; 7; 11; 13]

let test1_prime = prime 0 = []

let test2_prime = prime 1 = []

let test3_prime = prime 2 = [2]

let test4_prime = prime 10 = [2; 3; 5; 7]

let test5_prime = prime 13 = [2; 3; 5; 7; 11; 13]
