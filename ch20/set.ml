(* exer20.7 *)

type 'a t = 'a list

let empty = []

let singleton x = [x]

let rec remove x ys =
  match ys with
  | [] -> []
  | y' :: ys' ->
      if x = y' then
        remove x ys'
      else
        y' :: remove x ys'

let rec remove_dup xs =
  match xs with
  | [] -> []
  | x :: xs' -> x :: remove_dup (remove x xs')

let union xs ys = remove_dup (List.append xs ys)

let inter xs ys =
  let rec inter_sub xs ys =
    match xs with
    | [] -> []
    | x' :: xs' ->
        if List.mem x' ys then
          x' :: inter_sub xs' ys
        else
          inter_sub xs' ys
  in
  remove_dup (inter_sub xs ys)

let diff xs ys =
  let rec diff_sub xs ys =
    match xs with
    | [] -> []
    | x' :: xs' ->
        if List.mem x' ys then
          diff_sub xs' ys
        else
          x' :: diff_sub xs' ys
  in
  remove_dup (diff_sub xs ys)

let mem x xs = List.mem x xs

(* テスト *)
let empty_test1 = empty = []

let singleton_test1 = singleton 1 = [1]

let singleton_test2 = singleton "a" = ["a"]

let union_test1 = union [] [] = []

let union_test2 = union [1; 2] [] = [1; 2]

let union_test3 = union [] [1; 2] = [1; 2]

let union_test4 = union [1; 2; 3] [2; 3; 4] = [1; 2; 3; 4]

(* let union_test5 = union [3; 1; 2; 1] [2; 4; 3] = [1; 2; 3; 4] *)
let union_test5 = union [3; 1; 2; 1] [2; 4; 3] = [3; 1; 2; 4]

let inter_test1 = inter [] [] = []

let inter_test2 = inter [1; 2] [] = []

let inter_test3 = inter [] [1; 2] = []

let inter_test4 = inter [1; 2; 3] [2; 3; 4] = [2; 3]

let inter_test5 = inter [1; 2; 2; 3] [2; 3; 4] = [2; 3]

let diff_test1 = diff [] [] = []

let diff_test2 = diff [1; 2] [] = [1; 2]

let diff_test3 = diff [] [1; 2] = []

let diff_test4 = diff [1; 2; 3; 4] [2; 4] = [1; 3]

let diff_test5 = diff [1; 2; 2; 3] [1; 3] = [2]

let mem_test1 = mem 1 [] = false

let mem_test2 = mem 1 [1; 2; 3] = true

let mem_test3 = mem 4 [1; 2; 3] = false
