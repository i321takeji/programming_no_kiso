(* exer 8.5 *)
(* 駅名の情報を格納するレコード型 *)
type ekimei_t = {
  kanji : string;
  kana : string;
  romaji : string;
  shozoku : string;
}

(* exer 8.6 *)
(* 目的：ekimei_t 型のデータを受け取り，「路線名，駅名 (かな)」 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei = match ekimei with
  { kanji; kana; shozoku } ->
    shozoku ^ "，" ^ kanji ^ "（" ^ kana ^ "）"

let test1_hyoji = hyoji { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸の内線"}
                    = "丸の内線，茗荷谷（みょうがだに）"
let test2_hyoji = hyoji {kanji = "新宿"; kana = "しんじゅく"; romaji = "shinjuku"; shozoku = "山手線"}
                    = "山手線，新宿（しんじゅく）"
let test3_hyoji = hyoji {kanji = "成城学園前"; kana = "せいじょうがくえんまえ"; romaji = "seijogakuenmae"; shozoku = "小田急線"}
                    = "小田急線，成城学園前（せいじょうがくえんまえ）"
let test4_hyoji = hyoji {kanji = ""; kana = ""; romaji = "unknown"; shozoku = "テスト線"}
                    = "テスト線，（）"

(* exer 8.7 *)
(* 駅と駅の接続情報を格納するレコード型 *)
type ekikan_t = {
  kiten : string;  (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string;  (* 経由する駅名 *)
  kyori : float;   (* 2駅間の距離 *)
  jikan : int;     (* 所要時間 *)
}
