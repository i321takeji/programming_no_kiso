(* ecer20.7 *)

type 'a t (* 要素の型 'a の集合の型 *)

val empty : 'a t (* 空集合 *)

val singleton : 'a -> 'a t (* 要素ひとつからなる集合 *)

val union : 'a t -> 'a t -> 'a t (* 和集合 *)

val inter : 'a t -> 'a t -> 'a t (* 共通集合 *)

val diff : 'a t -> 'a t -> 'a t (* 差集合 *)

val mem : 'a -> 'a t -> bool (* 要素が集合に入っているか *)
