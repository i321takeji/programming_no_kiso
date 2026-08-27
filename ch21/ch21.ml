(* 目的：ふたつ自然 m, n の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  print_string "m = " ;
  print_int m ;
  (* m の値を表示 *)
  print_string ", n = " ;
  print_int n ;
  (* n の値を表示 *)
  print_newline () ;
  (* 改行 *)
  if n = 0 then
    m
  else
    gcd n (m mod n)

(* exer21.2 *)
(* 目的：2 <= n の自然数のリストを受け取ったら，2 <= n 以下の素数のリストを返す関数 *)
(* sieve : int list -> int list *)
let rec sieve lst =
  print_string "lst length: " ;
  print_int (List.length lst) ;
  print_newline () ;
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
