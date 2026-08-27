(* sec19.3 の二分探索木を表すモジュールと同じ *)
type ('a, 'b) t
(* キーが 'a 型，価が 'b 型の木の型．型の中身は非公開 *)

val empty : ('a, 'b) t
(* 使い型: empty *)
(* 空の木 *)

val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
(* 使い方: insert tree key value *)
(* 木 tree にキー key と値 value を挿入した木を返す *)
(* キーがすでに存在していたら新しい値に置き換える *)

val search : ('a, 'b) t -> 'a -> 'b
(* 使い方: search tree key *)
(* 木 tree の中からキー key に対応する値を探して返す *)
(* 見つからなければ Not_found を raise する *)
