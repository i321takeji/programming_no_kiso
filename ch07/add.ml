(* 目的：ふたつの整数の組 pair を受け取りその要素の和を返す *)
(* add : int * int -> int *)
let add pair = match pair with a, b -> a + b

(* テスト *)
let add_test1 = add (0, 0) = 0

let add_test2 = add (3, 5) = 8

let add_test3 = add (3, -5) = -2
