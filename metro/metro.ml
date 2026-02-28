(* exer 8.5 *)
(* 駅名の情報を格納するレコード型 *)
type ekimei_t =
  { kanji : string; kana : string; romaji : string; shozoku : string }

(* exer 8.6 *)
(* 目的：ekimei_t 型のデータを受け取り，「路線名，駅名 (かな)」 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei =
  match ekimei with
  | { kanji; kana; shozoku } -> shozoku ^ "，" ^ kanji ^ "（" ^ kana ^ "）"

let test1_hyoji =
  hyoji
    { kanji = "茗荷谷"
    ; kana = "みょうがだに"
    ; romaji = "myogadani"
    ; shozoku = "丸の内線"
    }
  = "丸の内線，茗荷谷（みょうがだに）"

let test2_hyoji =
  hyoji
    { kanji = "新宿"; kana = "しんじゅく"; romaji = "shinjuku"; shozoku = "山手線" }
  = "山手線，新宿（しんじゅく）"

let test3_hyoji =
  hyoji
    { kanji = "成城学園前"
    ; kana = "せいじょうがくえんまえ"
    ; romaji = "seijogakuenmae"
    ; shozoku = "小田急線"
    }
  = "小田急線，成城学園前（せいじょうがくえんまえ）"

let test4_hyoji =
  hyoji { kanji = ""; kana = ""; romaji = "unknown"; shozoku = "テスト線" }
  = "テスト線，（）"

(* exer 8.7 *)
(* 駅と駅の接続情報を格納するレコード型 *)
type ekikan_t =
  { kiten : string (* 起点の駅名 *)
  ; shuten : string (* 終点の駅名 *)
  ; keiyu : string (* 経由する駅名 *)
  ; kyori : float (* 2駅間の距離 *)
  ; jikan : int (* 所要時間 *)
  }

(* exer9.9 *)
let global_ekimei_list =
  [ { kanji = "代々木上原"
    ; kana = "よよぎうえはら"
    ; romaji = "yoyogiuehara"
    ; shozoku = "千代田線"
    }
  ; { kanji = "代々木公園"
    ; kana = "よよぎこうえん"
    ; romaji = "yoyogikouen"
    ; shozoku = "千代田線"
    }
  ; { kanji = "明治神宮前"
    ; kana = "めいじじんぐうまえ"
    ; romaji = "meijijinguumae"
    ; shozoku = "千代田線"
    }
  ; { kanji = "表参道"
    ; kana = "おもてさんどう"
    ; romaji = "omotesandou"
    ; shozoku = "千代田線"
    }
  ; { kanji = "乃木坂"; kana = "のぎざか"; romaji = "nogizaka"; shozoku = "千代田線" }
  ; { kanji = "赤坂"; kana = "あかさか"; romaji = "akasaka"; shozoku = "千代田線" }
  ; { kanji = "国会議事堂前"
    ; kana = "こっかいぎじどうまえ"
    ; romaji = "kokkaigijidoumae"
    ; shozoku = "千代田線"
    }
  ; { kanji = "霞ヶ関"
    ; kana = "かすみがせき"
    ; romaji = "kasumigaseki"
    ; shozoku = "千代田線"
    }
  ; { kanji = "日比谷"; kana = "ひびや"; romaji = "hibiya"; shozoku = "千代田線" }
  ; { kanji = "二重橋前"
    ; kana = "にじゅうばしまえ"
    ; romaji = "nijuubasimae"
    ; shozoku = "千代田線"
    }
  ; { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "千代田線" }
  ; { kanji = "新御茶ノ水"
    ; kana = "しんおちゃのみず"
    ; romaji = "shin-ochanomizu"
    ; shozoku = "千代田線"
    }
  ; { kanji = "湯島"; kana = "ゆしま"; romaji = "yushima"; shozoku = "千代田線" }
  ; { kanji = "根津"; kana = "ねづ"; romaji = "nedu"; shozoku = "千代田線" }
  ; { kanji = "千駄木"; kana = "せんだぎ"; romaji = "sendagi"; shozoku = "千代田線" }
  ; { kanji = "西日暮里"
    ; kana = "にしにっぽり"
    ; romaji = "nishinippori"
    ; shozoku = "千代田線"
    }
  ; { kanji = "町屋"; kana = "まちや"; romaji = "machiya"; shozoku = "千代田線" }
  ; { kanji = "北千住"
    ; kana = "きたせんじゅ"
    ; romaji = "kitasenjyu"
    ; shozoku = "千代田線"
    }
  ; { kanji = "綾瀬"; kana = "あやせ"; romaji = "ayase"; shozoku = "千代田線" }
  ; { kanji = "北綾瀬"; kana = "きたあやせ"; romaji = "kitaayase"; shozoku = "千代田線" }
  ; { kanji = "浅草"; kana = "あさくさ"; romaji = "asakusa"; shozoku = "銀座線" }
  ; { kanji = "田原町"
    ; kana = "たわらまち"
    ; romaji = "tawaramachi"
    ; shozoku = "銀座線"
    }
  ; { kanji = "稲荷町"; kana = "いなりちょう"; romaji = "inaricho"; shozoku = "銀座線" }
  ; { kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "銀座線" }
  ; { kanji = "上野広小路"
    ; kana = "うえのひろこうじ"
    ; romaji = "uenohirokoji"
    ; shozoku = "銀座線"
    }
  ; { kanji = "末広町"
    ; kana = "すえひろちょう"
    ; romaji = "suehirocho"
    ; shozoku = "銀座線"
    }
  ; { kanji = "神田"; kana = "かんだ"; romaji = "kanda"; shozoku = "銀座線" }
  ; { kanji = "三越前"
    ; kana = "みつこしまえ"
    ; romaji = "mitsukoshimae"
    ; shozoku = "銀座線"
    }
  ; { kanji = "日本橋"; kana = "にほんばし"; romaji = "nihonbashi"; shozoku = "銀座線" }
  ; { kanji = "京橋"; kana = "きょうばし"; romaji = "kyobashi"; shozoku = "銀座線" }
  ; { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "銀座線" }
  ; { kanji = "新橋"; kana = "しんばし"; romaji = "shinbashi"; shozoku = "銀座線" }
  ; { kanji = "虎ノ門"; kana = "とらのもん"; romaji = "toranomon"; shozoku = "銀座線" }
  ; { kanji = "溜池山王"
    ; kana = "ためいけさんのう"
    ; romaji = "tameikesannou"
    ; shozoku = "銀座線"
    }
  ; { kanji = "赤坂見附"
    ; kana = "あかさかみつけ"
    ; romaji = "akasakamitsuke"
    ; shozoku = "銀座線"
    }
  ; { kanji = "青山一丁目"
    ; kana = "あおやまいっちょうめ"
    ; romaji = "aoyamaicchome"
    ; shozoku = "銀座線"
    }
  ; { kanji = "外苑前"; kana = "がいえんまえ"; romaji = "gaienmae"; shozoku = "銀座線" }
  ; { kanji = "表参道"
    ; kana = "おもてさんどう"
    ; romaji = "omotesando"
    ; shozoku = "銀座線"
    }
  ; { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "銀座線" }
  ; { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "半蔵門線" }
  ; { kanji = "表参道"
    ; kana = "おもてさんどう"
    ; romaji = "omotesandou"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "青山一丁目"
    ; kana = "あおやまいっちょうめ"
    ; romaji = "aoyama-itchome"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "永田町"
    ; kana = "ながたちょう"
    ; romaji = "nagatacho"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "半蔵門"; kana = "はんぞうもん"; romaji = "hanzomon"; shozoku = "半蔵門線" }
  ; { kanji = "九段下"
    ; kana = "くだんした"
    ; romaji = "kudanshita"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "神保町"
    ; kana = "じんぼうちょう"
    ; romaji = "jinbocho"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "半蔵門線" }
  ; { kanji = "三越前"
    ; kana = "みつこしまえ"
    ; romaji = "mitsukoshimae"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "水天宮前"
    ; kana = "すいてんぐうまえ"
    ; romaji = "suitengumae"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "清澄白河"
    ; kana = "きよすみしらかわ"
    ; romaji = "kiyosumi-shirakawa"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "住吉"; kana = "すみよし"; romaji = "sumiyoshi"; shozoku = "半蔵門線" }
  ; { kanji = "錦糸町"
    ; kana = "きんしちょう"
    ; romaji = "kinshicho"
    ; shozoku = "半蔵門線"
    }
  ; { kanji = "押上"; kana = "おしあげ"; romaji = "oshiage"; shozoku = "半蔵門線" }
  ; { kanji = "中目黒"
    ; kana = "なかめぐろ"
    ; romaji = "nakameguro"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "恵比寿"; kana = "えびす"; romaji = "ebisu"; shozoku = "日比谷線" }
  ; { kanji = "広尾"; kana = "ひろお"; romaji = "hiro"; shozoku = "日比谷線" }
  ; { kanji = "六本木"; kana = "ろっぽんぎ"; romaji = "roppongi"; shozoku = "日比谷線" }
  ; { kanji = "神谷町"
    ; kana = "かみやちょう"
    ; romaji = "kamiyacho"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "霞ヶ関"
    ; kana = "かすみがせき"
    ; romaji = "kasumigaseki"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "日比谷"; kana = "ひびや"; romaji = "hibiya"; shozoku = "日比谷線" }
  ; { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "日比谷線" }
  ; { kanji = "東銀座"
    ; kana = "ひがしぎんざ"
    ; romaji = "higashiginza"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "築地"; kana = "つきじ"; romaji = "tsukiji"; shozoku = "日比谷線" }
  ; { kanji = "八丁堀"
    ; kana = "はっちょうぼり"
    ; romaji = "hacchobori"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "茅場町"
    ; kana = "かやばちょう"
    ; romaji = "kayabacho"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "人形町"
    ; kana = "にんぎょうちょう"
    ; romaji = "ningyomachi"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "小伝馬町"
    ; kana = "こでんまちょう"
    ; romaji = "kodemmacho"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "秋葉原"; kana = "あきはばら"; romaji = "akihabara"; shozoku = "日比谷線" }
  ; { kanji = "仲御徒町"
    ; kana = "なかおかちまち"
    ; romaji = "nakaokachimachi"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "日比谷線" }
  ; { kanji = "入谷"; kana = "いりや"; romaji = "iriya"; shozoku = "日比谷線" }
  ; { kanji = "三ノ輪"; kana = "みのわ"; romaji = "minowa"; shozoku = "日比谷線" }
  ; { kanji = "南千住"
    ; kana = "みなみせんじゅ"
    ; romaji = "minamisenju"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "北千住"
    ; kana = "きたせんじゅ"
    ; romaji = "kitasenju"
    ; shozoku = "日比谷線"
    }
  ; { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" }
  ; { kanji = "新大塚"
    ; kana = "しんおおつか"
    ; romaji = "shinotsuka"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "茗荷谷"
    ; kana = "みょうがだに"
    ; romaji = "myogadani"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" }
  ; { kanji = "本郷三丁目"
    ; kana = "ほんごうさんちょうめ"
    ; romaji = "hongosanchome"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "御茶ノ水"
    ; kana = "おちゃのみず"
    ; romaji = "ochanomizu"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "淡路町"; kana = "あわじちょう"; romaji = "awajicho"; shozoku = "丸ノ内線" }
  ; { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "丸ノ内線" }
  ; { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" }
  ; { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "丸ノ内線" }
  ; { kanji = "霞ヶ関"
    ; kana = "かすみがせき"
    ; romaji = "kasumigaseki"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "国会議事堂前"
    ; kana = "こっかいぎじどうまえ"
    ; romaji = "kokkaigijidomae"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "赤坂見附"
    ; kana = "あかさかみつけ"
    ; romaji = "akasakamitsuke"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "四ツ谷"; kana = "よつや"; romaji = "yotsuya"; shozoku = "丸ノ内線" }
  ; { kanji = "四谷三丁目"
    ; kana = "よつやさんちょうめ"
    ; romaji = "yotsuyasanchome"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "新宿御苑前"
    ; kana = "しんじゅくぎょえんまえ"
    ; romaji = "shinjuku-gyoemmae"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "新宿三丁目"
    ; kana = "しんじゅくさんちょうめ"
    ; romaji = "shinjuku-sanchome"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "新宿"; kana = "しんじゅく"; romaji = "shinjuku"; shozoku = "丸ノ内線" }
  ; { kanji = "西新宿"
    ; kana = "にししんじゅく"
    ; romaji = "nishi-shinjuku"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "中野坂上"
    ; kana = "なかのさかうえ"
    ; romaji = "nakano-sakaue"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "新中野"
    ; kana = "しんなかの"
    ; romaji = "shin-nakano"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "東高円寺"
    ; kana = "ひがしこうえんじ"
    ; romaji = "higashi-koenji"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "新高円寺"
    ; kana = "しんこうえんじ"
    ; romaji = "shin-koenji"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "南阿佐ヶ谷"
    ; kana = "みなみあさがや"
    ; romaji = "minami-asagaya"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "荻窪"; kana = "おぎくぼ"; romaji = "ogikubo"; shozoku = "丸ノ内線" }
  ; { kanji = "中野新橋"
    ; kana = "なかのしんばし"
    ; romaji = "nakano-shimbashi"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "中野富士見町"
    ; kana = "なかのふじみちょう"
    ; romaji = "nakano-fujimicho"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "方南町"
    ; kana = "ほうなんちょう"
    ; romaji = "honancho"
    ; shozoku = "丸ノ内線"
    }
  ; { kanji = "四ツ谷"; kana = "よつや"; romaji = "yotsuya"; shozoku = "南北線" }
  ; { kanji = "永田町"; kana = "ながたちょう"; romaji = "nagatacho"; shozoku = "南北線" }
  ; { kanji = "溜池山王"
    ; kana = "ためいけさんのう"
    ; romaji = "tameikesanno"
    ; shozoku = "南北線"
    }
  ; { kanji = "六本木一丁目"
    ; kana = "ろっぽんぎいっちょうめ"
    ; romaji = "roppongiitchome"
    ; shozoku = "南北線"
    }
  ; { kanji = "麻布十番"
    ; kana = "あざぶじゅうばん"
    ; romaji = "azabujuban"
    ; shozoku = "南北線"
    }
  ; { kanji = "白金高輪"
    ; kana = "しろかねたかなわ"
    ; romaji = "shirokanetakanawa"
    ; shozoku = "南北線"
    }
  ; { kanji = "白金台"
    ; kana = "しろかねだい"
    ; romaji = "shirokanedai"
    ; shozoku = "南北線"
    }
  ; { kanji = "目黒"; kana = "めぐろ"; romaji = "meguro"; shozoku = "南北線" }
  ; { kanji = "市ヶ谷"; kana = "いちがや"; romaji = "ichigaya"; shozoku = "南北線" }
  ; { kanji = "飯田橋"; kana = "いいだばし"; romaji = "idabashi"; shozoku = "南北線" }
  ; { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "南北線" }
  ; { kanji = "東大前"; kana = "とうだいまえ"; romaji = "todaimae"; shozoku = "南北線" }
  ; { kanji = "本駒込"
    ; kana = "ほんこまごめ"
    ; romaji = "honkomagome"
    ; shozoku = "南北線"
    }
  ; { kanji = "駒込"; kana = "こまごめ"; romaji = "komagome"; shozoku = "南北線" }
  ; { kanji = "西ヶ原"
    ; kana = "にしがはら"
    ; romaji = "nishigahara"
    ; shozoku = "南北線"
    }
  ; { kanji = "王子"; kana = "おうじ"; romaji = "oji"; shozoku = "南北線" }
  ; { kanji = "王子神谷"
    ; kana = "おうじかみや"
    ; romaji = "ojikamiya"
    ; shozoku = "南北線"
    }
  ; { kanji = "志茂"; kana = "しも"; romaji = "shimo"; shozoku = "南北線" }
  ; { kanji = "赤羽岩淵"
    ; kana = "あかばねいわぶち"
    ; romaji = "akabaneiwabuchi"
    ; shozoku = "南北線"
    }
  ; { kanji = "西船橋"
    ; kana = "にしふなばし"
    ; romaji = "nishi-funabashi"
    ; shozoku = "東西線"
    }
  ; { kanji = "原木中山"
    ; kana = "ばらきなかやま"
    ; romaji = "baraki-nakayama"
    ; shozoku = "東西線"
    }
  ; { kanji = "妙典"; kana = "みょうでん"; romaji = "myoden"; shozoku = "東西線" }
  ; { kanji = "行徳"; kana = "ぎょうとく"; romaji = "gyotoku"; shozoku = "東西線" }
  ; { kanji = "南行徳"
    ; kana = "みなみぎょうとく"
    ; romaji = "minami-gyotoku"
    ; shozoku = "東西線"
    }
  ; { kanji = "浦安"; kana = "うらやす"; romaji = "urayasu"; shozoku = "東西線" }
  ; { kanji = "葛西"; kana = "かさい"; romaji = "kasai"; shozoku = "東西線" }
  ; { kanji = "西葛西"
    ; kana = "にしかさい"
    ; romaji = "nishi-kasai"
    ; shozoku = "東西線"
    }
  ; { kanji = "南砂町"
    ; kana = "みなみすなまち"
    ; romaji = "minami-sunamachi"
    ; shozoku = "東西線"
    }
  ; { kanji = "東陽町"
    ; kana = "とうようちょう"
    ; romaji = "touyoucho"
    ; shozoku = "東西線"
    }
  ; { kanji = "木場"; kana = "きば"; romaji = "kiba"; shozoku = "東西線" }
  ; { kanji = "門前仲町"
    ; kana = "もんぜんなかちょう"
    ; romaji = "monzen-nakacho"
    ; shozoku = "東西線"
    }
  ; { kanji = "茅場町"; kana = "かやばちょう"; romaji = "kayabacho"; shozoku = "東西線" }
  ; { kanji = "日本橋"; kana = "にほんばし"; romaji = "nihonbashi"; shozoku = "東西線" }
  ; { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "東西線" }
  ; { kanji = "竹橋"; kana = "たけばし"; romaji = "takebashi"; shozoku = "東西線" }
  ; { kanji = "九段下"; kana = "くだんした"; romaji = "kudanshita"; shozoku = "東西線" }
  ; { kanji = "飯田橋"; kana = "いいだばし"; romaji = "iidabashi"; shozoku = "東西線" }
  ; { kanji = "神楽坂"; kana = "かぐらざか"; romaji = "kagurazaka"; shozoku = "東西線" }
  ; { kanji = "早稲田"; kana = "わせだ"; romaji = "waseda"; shozoku = "東西線" }
  ; { kanji = "高田馬場"
    ; kana = "たかだのばば"
    ; romaji = "takadanobaba"
    ; shozoku = "東西線"
    }
  ; { kanji = "落合"; kana = "おちあい"; romaji = "ochiai"; shozoku = "東西線" }
  ; { kanji = "中野"; kana = "なかの"; romaji = "nakano"; shozoku = "東西線" }
  ; { romaji = "shinkiba"; kana = "しんきば"; kanji = "新木場"; shozoku = "有楽町線" }
  ; { romaji = "tatsumi"; kana = "たつみ"; kanji = "辰巳"; shozoku = "有楽町線" }
  ; { romaji = "toyosu"; kana = "とよす"; kanji = "豊洲"; shozoku = "有楽町線" }
  ; { romaji = "tsukishima"; kana = "つきしま"; kanji = "月島"; shozoku = "有楽町線" }
  ; { romaji = "shintomityou"
    ; kana = "しんとみちょう"
    ; kanji = "新富町"
    ; shozoku = "有楽町線"
    }
  ; { romaji = "ginzaittyoume"
    ; kana = "ぎんざいっちょうめ"
    ; kanji = "銀座一丁目"
    ; shozoku = "有楽町線"
    }
  ; { romaji = "yuurakutyou"
    ; kana = "ゆうらくちょう"
    ; kanji = "有楽町"
    ; shozoku = "有楽町線"
    }
  ; { romaji = "sakuradamon"
    ; kana = "さくらだもん"
    ; kanji = "桜田門"
    ; shozoku = "有楽町線"
    }
  ; { romaji = "nagatacho"
    ; kana = "ながたちょう"
    ; kanji = "永田町"
    ; shozoku = "有楽町線"
    }
  ; { romaji = "koujimachi"; kana = "こうじまち"; kanji = "麹町"; shozoku = "有楽町線" }
  ; { romaji = "ichigaya"; kana = "いちがや"; kanji = "市ヶ谷"; shozoku = "有楽町線" }
  ; { romaji = "iidabashi"; kana = "いいだばし"; kanji = "飯田橋"; shozoku = "有楽町線" }
  ; { kanji = "江戸川橋"
    ; kana = "えどがわばし"
    ; romaji = "edogawabasi"
    ; shozoku = "有楽町線"
    }
  ; { kanji = "護国寺"; kana = "ごこくじ"; romaji = "gokokuji"; shozoku = "有楽町線" }
  ; { kanji = "東池袋"
    ; kana = "ひがしいけぶくろ"
    ; romaji = "higasiikebukuro"
    ; shozoku = "有楽町線"
    }
  ; { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "有楽町線" }
  ; { kanji = "要町"
    ; kana = "かなめちょう"
    ; romaji = "kanametyou"
    ; shozoku = "有楽町線"
    }
  ; { kanji = "千川"; kana = "せんかわ"; romaji = "senkawa"; shozoku = "有楽町線" }
  ; { kanji = "小竹向原"
    ; kana = "こたけむかいはら"
    ; romaji = "kotakemukaihara"
    ; shozoku = "有楽町線"
    }
  ; { kanji = "氷川台"; kana = "ひかわだい"; romaji = "hikawadai"; shozoku = "有楽町線" }
  ; { kanji = "平和台"; kana = "へいわだい"; romaji = "heiwadai"; shozoku = "有楽町線" }
  ; { kanji = "営団赤塚"
    ; kana = "えいだんあかつか"
    ; romaji = "eidanakakuka"
    ; shozoku = "有楽町線"
    }
  ; { kanji = "営団成増"
    ; kana = "えいだんなります"
    ; romaji = "eidannarimasu"
    ; shozoku = "有楽町線"
    }
  ; { kanji = "和光市"; kana = "わこうし"; romaji = "wakousi"; shozoku = "有楽町線" } ]

(* exer9.10 *)
let global_ekikan_list =
  [ { kiten = "代々木上原"
    ; shuten = "代々木公園"
    ; keiyu = "千代田線"
    ; kyori = 1.0
    ; jikan = 2
    }
  ; { kiten = "代々木公園"
    ; shuten = "明治神宮前"
    ; keiyu = "千代田線"
    ; kyori = 1.2
    ; jikan = 2
    }
  ; { kiten = "明治神宮前"
    ; shuten = "表参道"
    ; keiyu = "千代田線"
    ; kyori = 0.9
    ; jikan = 2
    }
  ; { kiten = "表参道"; shuten = "乃木坂"; keiyu = "千代田線"; kyori = 1.4; jikan = 3 }
  ; { kiten = "乃木坂"; shuten = "赤坂"; keiyu = "千代田線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "赤坂"
    ; shuten = "国会議事堂前"
    ; keiyu = "千代田線"
    ; kyori = 0.8
    ; jikan = 1
    }
  ; { kiten = "国会議事堂前"
    ; shuten = "霞ヶ関"
    ; keiyu = "千代田線"
    ; kyori = 0.7
    ; jikan = 1
    }
  ; { kiten = "霞ヶ関"; shuten = "日比谷"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "日比谷"
    ; shuten = "二重橋前"
    ; keiyu = "千代田線"
    ; kyori = 0.7
    ; jikan = 1
    }
  ; { kiten = "二重橋前"
    ; shuten = "大手町"
    ; keiyu = "千代田線"
    ; kyori = 0.7
    ; jikan = 1
    }
  ; { kiten = "大手町"
    ; shuten = "新御茶ノ水"
    ; keiyu = "千代田線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "新御茶ノ水"
    ; shuten = "湯島"
    ; keiyu = "千代田線"
    ; kyori = 1.2
    ; jikan = 2
    }
  ; { kiten = "湯島"; shuten = "根津"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "根津"; shuten = "千駄木"; keiyu = "千代田線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "千駄木"
    ; shuten = "西日暮里"
    ; keiyu = "千代田線"
    ; kyori = 0.9
    ; jikan = 1
    }
  ; { kiten = "西日暮里"; shuten = "町屋"; keiyu = "千代田線"; kyori = 1.7; jikan = 2 }
  ; { kiten = "町屋"; shuten = "北千住"; keiyu = "千代田線"; kyori = 2.6; jikan = 3 }
  ; { kiten = "北千住"; shuten = "綾瀬"; keiyu = "千代田線"; kyori = 2.5; jikan = 3 }
  ; { kiten = "綾瀬"; shuten = "北綾瀬"; keiyu = "千代田線"; kyori = 2.1; jikan = 4 }
  ; { kiten = "浅草"; shuten = "田原町"; keiyu = "銀座線"; kyori = 0.8; jikan = 2 }
  ; { kiten = "田原町"; shuten = "稲荷町"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 }
  ; { kiten = "稲荷町"; shuten = "上野"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 }
  ; { kiten = "上野"; shuten = "上野広小路"; keiyu = "銀座線"; kyori = 0.5; jikan = 2 }
  ; { kiten = "上野広小路"
    ; shuten = "末広町"
    ; keiyu = "銀座線"
    ; kyori = 0.6
    ; jikan = 1
    }
  ; { kiten = "末広町"; shuten = "神田"; keiyu = "銀座線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "神田"; shuten = "三越前"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 }
  ; { kiten = "三越前"; shuten = "日本橋"; keiyu = "銀座線"; kyori = 0.6; jikan = 2 }
  ; { kiten = "日本橋"; shuten = "京橋"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 }
  ; { kiten = "京橋"; shuten = "銀座"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 }
  ; { kiten = "銀座"; shuten = "新橋"; keiyu = "銀座線"; kyori = 0.9; jikan = 2 }
  ; { kiten = "新橋"; shuten = "虎ノ門"; keiyu = "銀座線"; kyori = 0.8; jikan = 2 }
  ; { kiten = "虎ノ門"; shuten = "溜池山王"; keiyu = "銀座線"; kyori = 0.6; jikan = 2 }
  ; { kiten = "溜池山王"
    ; shuten = "赤坂見附"
    ; keiyu = "銀座線"
    ; kyori = 0.9
    ; jikan = 2
    }
  ; { kiten = "赤坂見附"
    ; shuten = "青山一丁目"
    ; keiyu = "銀座線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "青山一丁目"
    ; shuten = "外苑前"
    ; keiyu = "銀座線"
    ; kyori = 0.7
    ; jikan = 2
    }
  ; { kiten = "外苑前"; shuten = "表参道"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 }
  ; { kiten = "表参道"; shuten = "渋谷"; keiyu = "銀座線"; kyori = 1.3; jikan = 1 }
  ; { kiten = "渋谷"; shuten = "表参道"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 }
  ; { kiten = "表参道"
    ; shuten = "青山一丁目"
    ; keiyu = "半蔵門線"
    ; kyori = 1.4
    ; jikan = 2
    }
  ; { kiten = "青山一丁目"
    ; shuten = "永田町"
    ; keiyu = "半蔵門線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "永田町"; shuten = "半蔵門"; keiyu = "半蔵門線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "半蔵門"; shuten = "九段下"; keiyu = "半蔵門線"; kyori = 1.6; jikan = 2 }
  ; { kiten = "九段下"; shuten = "神保町"; keiyu = "半蔵門線"; kyori = 0.4; jikan = 1 }
  ; { kiten = "神保町"; shuten = "大手町"; keiyu = "半蔵門線"; kyori = 1.7; jikan = 3 }
  ; { kiten = "大手町"; shuten = "三越前"; keiyu = "半蔵門線"; kyori = 0.7; jikan = 1 }
  ; { kiten = "三越前"
    ; shuten = "水天宮前"
    ; keiyu = "半蔵門線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "水天宮前"
    ; shuten = "清澄白河"
    ; keiyu = "半蔵門線"
    ; kyori = 1.7
    ; jikan = 3
    }
  ; { kiten = "清澄白河"; shuten = "住吉"; keiyu = "半蔵門線"; kyori = 1.9; jikan = 3 }
  ; { kiten = "住吉"; shuten = "錦糸町"; keiyu = "半蔵門線"; kyori = 1.; jikan = 2 }
  ; { kiten = "錦糸町"; shuten = "押上"; keiyu = "半蔵門線"; kyori = 1.4; jikan = 2 }
  ; { kiten = "中目黒"; shuten = "恵比寿"; keiyu = "日比谷線"; kyori = 1.; jikan = 2 }
  ; { kiten = "恵比寿"; shuten = "広尾"; keiyu = "日比谷線"; kyori = 1.5; jikan = 3 }
  ; { kiten = "広尾"; shuten = "六本木"; keiyu = "日比谷線"; kyori = 1.7; jikan = 3 }
  ; { kiten = "六本木"; shuten = "神谷町"; keiyu = "日比谷線"; kyori = 1.5; jikan = 3 }
  ; { kiten = "神谷町"; shuten = "霞ヶ関"; keiyu = "日比谷線"; kyori = 1.3; jikan = 2 }
  ; { kiten = "霞ヶ関"; shuten = "日比谷"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "日比谷"; shuten = "銀座"; keiyu = "日比谷線"; kyori = 0.4; jikan = 1 }
  ; { kiten = "銀座"; shuten = "東銀座"; keiyu = "日比谷線"; kyori = 0.4; jikan = 1 }
  ; { kiten = "東銀座"; shuten = "築地"; keiyu = "日比谷線"; kyori = 0.6; jikan = 2 }
  ; { kiten = "築地"; shuten = "八丁堀"; keiyu = "日比谷線"; kyori = 1.; jikan = 2 }
  ; { kiten = "八丁堀"; shuten = "茅場町"; keiyu = "日比谷線"; kyori = 0.5; jikan = 1 }
  ; { kiten = "茅場町"; shuten = "人形町"; keiyu = "日比谷線"; kyori = 0.9; jikan = 2 }
  ; { kiten = "人形町"
    ; shuten = "小伝馬町"
    ; keiyu = "日比谷線"
    ; kyori = 0.6
    ; jikan = 1
    }
  ; { kiten = "小伝馬町"
    ; shuten = "秋葉原"
    ; keiyu = "日比谷線"
    ; kyori = 0.9
    ; jikan = 2
    }
  ; { kiten = "秋葉原"; shuten = "仲御徒町"; keiyu = "日比谷線"; kyori = 1.; jikan = 1 }
  ; { kiten = "仲御徒町"; shuten = "上野"; keiyu = "日比谷線"; kyori = 0.5; jikan = 1 }
  ; { kiten = "上野"; shuten = "入谷"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "入谷"; shuten = "三ノ輪"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "三ノ輪"; shuten = "南千住"; keiyu = "日比谷線"; kyori = 0.8; jikan = 2 }
  ; { kiten = "南千住"; shuten = "北千住"; keiyu = "日比谷線"; kyori = 1.8; jikan = 3 }
  ; { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 }
  ; { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 }
  ; { kiten = "後楽園"
    ; shuten = "本郷三丁目"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.8
    ; jikan = 1
    }
  ; { kiten = "本郷三丁目"
    ; shuten = "御茶ノ水"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.8
    ; jikan = 1
    }
  ; { kiten = "御茶ノ水"
    ; shuten = "淡路町"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.8
    ; jikan = 1
    }
  ; { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 }
  ; { kiten = "大手町"; shuten = "東京"; keiyu = "丸ノ内線"; kyori = 0.6; jikan = 1 }
  ; { kiten = "東京"; shuten = "銀座"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "銀座"; shuten = "霞ヶ関"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "霞ヶ関"
    ; shuten = "国会議事堂前"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.7
    ; jikan = 1
    }
  ; { kiten = "国会議事堂前"
    ; shuten = "赤坂見附"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.9
    ; jikan = 2
    }
  ; { kiten = "赤坂見附"
    ; shuten = "四ツ谷"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "四ツ谷"
    ; shuten = "四谷三丁目"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.0
    ; jikan = 2
    }
  ; { kiten = "四谷三丁目"
    ; shuten = "新宿御苑前"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.9
    ; jikan = 1
    }
  ; { kiten = "新宿御苑前"
    ; shuten = "新宿三丁目"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.7
    ; jikan = 1
    }
  ; { kiten = "新宿三丁目"
    ; shuten = "新宿"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.3
    ; jikan = 1
    }
  ; { kiten = "新宿"; shuten = "西新宿"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 }
  ; { kiten = "西新宿"
    ; shuten = "中野坂上"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.1
    ; jikan = 2
    }
  ; { kiten = "中野坂上"
    ; shuten = "新中野"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.1
    ; jikan = 2
    }
  ; { kiten = "新中野"
    ; shuten = "東高円寺"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.0
    ; jikan = 1
    }
  ; { kiten = "東高円寺"
    ; shuten = "新高円寺"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.9
    ; jikan = 1
    }
  ; { kiten = "新高円寺"
    ; shuten = "南阿佐ヶ谷"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.2
    ; jikan = 2
    }
  ; { kiten = "南阿佐ヶ谷"
    ; shuten = "荻窪"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.5
    ; jikan = 2
    }
  ; { kiten = "中野坂上"
    ; shuten = "中野新橋"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "中野新橋"
    ; shuten = "中野富士見町"
    ; keiyu = "丸ノ内線"
    ; kyori = 0.6
    ; jikan = 1
    }
  ; { kiten = "中野富士見町"
    ; shuten = "方南町"
    ; keiyu = "丸ノ内線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "市ヶ谷"; shuten = "四ツ谷"; keiyu = "南北線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "四ツ谷"; shuten = "永田町"; keiyu = "南北線"; kyori = 1.3; jikan = 3 }
  ; { kiten = "永田町"; shuten = "溜池山王"; keiyu = "南北線"; kyori = 0.9; jikan = 1 }
  ; { kiten = "溜池山王"
    ; shuten = "六本木一丁目"
    ; keiyu = "南北線"
    ; kyori = 0.9
    ; jikan = 2
    }
  ; { kiten = "六本木一丁目"
    ; shuten = "麻布十番"
    ; keiyu = "南北線"
    ; kyori = 1.2
    ; jikan = 2
    }
  ; { kiten = "麻布十番"
    ; shuten = "白金高輪"
    ; keiyu = "南北線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "白金高輪"; shuten = "白金台"; keiyu = "南北線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "白金台"; shuten = "目黒"; keiyu = "南北線"; kyori = 1.3; jikan = 2 }
  ; { kiten = "市ヶ谷"; shuten = "飯田橋"; keiyu = "南北線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "飯田橋"; shuten = "後楽園"; keiyu = "南北線"; kyori = 1.4; jikan = 2 }
  ; { kiten = "後楽園"; shuten = "東大前"; keiyu = "南北線"; kyori = 1.3; jikan = 3 }
  ; { kiten = "東大前"; shuten = "本駒込"; keiyu = "南北線"; kyori = 0.9; jikan = 2 }
  ; { kiten = "本駒込"; shuten = "駒込"; keiyu = "南北線"; kyori = 1.4; jikan = 2 }
  ; { kiten = "駒込"; shuten = "西ヶ原"; keiyu = "南北線"; kyori = 1.4; jikan = 2 }
  ; { kiten = "西ヶ原"; shuten = "王子"; keiyu = "南北線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "王子"; shuten = "王子神谷"; keiyu = "南北線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "王子神谷"; shuten = "志茂"; keiyu = "南北線"; kyori = 1.6; jikan = 3 }
  ; { kiten = "志茂"; shuten = "赤羽岩淵"; keiyu = "南北線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "西船橋"; shuten = "原木中山"; keiyu = "東西線"; kyori = 1.9; jikan = 3 }
  ; { kiten = "原木中山"; shuten = "妙典"; keiyu = "東西線"; kyori = 2.1; jikan = 2 }
  ; { kiten = "妙典"; shuten = "行徳"; keiyu = "東西線"; kyori = 1.3; jikan = 2 }
  ; { kiten = "行徳"; shuten = "南行徳"; keiyu = "東西線"; kyori = 1.5; jikan = 2 }
  ; { kiten = "南行徳"; shuten = "浦安"; keiyu = "東西線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "浦安"; shuten = "葛西"; keiyu = "東西線"; kyori = 1.9; jikan = 2 }
  ; { kiten = "葛西"; shuten = "西葛西"; keiyu = "東西線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "西葛西"; shuten = "南砂町"; keiyu = "東西線"; kyori = 2.7; jikan = 2 }
  ; { kiten = "南砂町"; shuten = "東陽町"; keiyu = "東西線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "東陽町"; shuten = "木場"; keiyu = "東西線"; kyori = 0.9; jikan = 1 }
  ; { kiten = "木場"; shuten = "門前仲町"; keiyu = "東西線"; kyori = 1.1; jikan = 1 }
  ; { kiten = "門前仲町"; shuten = "茅場町"; keiyu = "東西線"; kyori = 1.8; jikan = 2 }
  ; { kiten = "茅場町"; shuten = "日本橋"; keiyu = "東西線"; kyori = 0.5; jikan = 1 }
  ; { kiten = "日本橋"; shuten = "大手町"; keiyu = "東西線"; kyori = 0.8; jikan = 1 }
  ; { kiten = "大手町"; shuten = "竹橋"; keiyu = "東西線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "竹橋"; shuten = "九段下"; keiyu = "東西線"; kyori = 1.0; jikan = 1 }
  ; { kiten = "九段下"; shuten = "飯田橋"; keiyu = "東西線"; kyori = 0.7; jikan = 1 }
  ; { kiten = "飯田橋"; shuten = "神楽坂"; keiyu = "東西線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "神楽坂"; shuten = "早稲田"; keiyu = "東西線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "早稲田"; shuten = "高田馬場"; keiyu = "東西線"; kyori = 1.7; jikan = 3 }
  ; { kiten = "高田馬場"; shuten = "落合"; keiyu = "東西線"; kyori = 1.9; jikan = 3 }
  ; { kiten = "落合"; shuten = "中野"; keiyu = "東西線"; kyori = 2.0; jikan = 3 }
  ; { kiten = "新木場"; shuten = "辰巳"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 }
  ; { kiten = "辰巳"; shuten = "豊洲"; keiyu = "有楽町線"; kyori = 1.7; jikan = 2 }
  ; { kiten = "豊洲"; shuten = "月島"; keiyu = "有楽町線"; kyori = 1.4; jikan = 2 }
  ; { kiten = "月島"; shuten = "新富町"; keiyu = "有楽町線"; kyori = 1.3; jikan = 2 }
  ; { kiten = "新富町"
    ; shuten = "銀座一丁目"
    ; keiyu = "有楽町線"
    ; kyori = 0.7
    ; jikan = 1
    }
  ; { kiten = "銀座一丁目"
    ; shuten = "有楽町"
    ; keiyu = "有楽町線"
    ; kyori = 0.5
    ; jikan = 1
    }
  ; { kiten = "有楽町"; shuten = "桜田門"; keiyu = "有楽町線"; kyori = 1.0; jikan = 1 }
  ; { kiten = "桜田門"; shuten = "永田町"; keiyu = "有楽町線"; kyori = 0.9; jikan = 2 }
  ; { kiten = "永田町"; shuten = "麹町"; keiyu = "有楽町線"; kyori = 0.9; jikan = 1 }
  ; { kiten = "麹町"; shuten = "市ヶ谷"; keiyu = "有楽町線"; kyori = 0.9; jikan = 1 }
  ; { kiten = "市ヶ谷"; shuten = "飯田橋"; keiyu = "有楽町線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "飯田橋"
    ; shuten = "江戸川橋"
    ; keiyu = "有楽町線"
    ; kyori = 1.6
    ; jikan = 3
    }
  ; { kiten = "江戸川橋"
    ; shuten = "護国寺"
    ; keiyu = "有楽町線"
    ; kyori = 1.3
    ; jikan = 2
    }
  ; { kiten = "護国寺"; shuten = "東池袋"; keiyu = "有楽町線"; kyori = 1.1; jikan = 2 }
  ; { kiten = "東池袋"; shuten = "池袋"; keiyu = "有楽町線"; kyori = 2.0; jikan = 2 }
  ; { kiten = "池袋"; shuten = "要町"; keiyu = "有楽町線"; kyori = 1.2; jikan = 2 }
  ; { kiten = "要町"; shuten = "千川"; keiyu = "有楽町線"; kyori = 1.0; jikan = 1 }
  ; { kiten = "千川"; shuten = "小竹向原"; keiyu = "有楽町線"; kyori = 1.0; jikan = 2 }
  ; { kiten = "小竹向原"
    ; shuten = "氷川台"
    ; keiyu = "有楽町線"
    ; kyori = 1.5
    ; jikan = 2
    }
  ; { kiten = "氷川台"; shuten = "平和台"; keiyu = "有楽町線"; kyori = 1.4; jikan = 2 }
  ; { kiten = "平和台"
    ; shuten = "営団赤塚"
    ; keiyu = "有楽町線"
    ; kyori = 1.8
    ; jikan = 2
    }
  ; { kiten = "営団赤塚"
    ; shuten = "営団成増"
    ; keiyu = "有楽町線"
    ; kyori = 1.5
    ; jikan = 2
    }
  ; { kiten = "営団成増"
    ; shuten = "和光市"
    ; keiyu = "有楽町線"
    ; kyori = 2.1
    ; jikan = 3
    } ]

(* exer10.10 *)
(* 目的：ローマ字の駅名と駅名リストを受け取り，その駅の漢字表記を文字列で返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji eki_romaji ekimei_lst =
  match ekimei_lst with
  | [] -> ""
  | { romaji; kanji } :: rest ->
      if eki_romaji = romaji then
        kanji
      else
        romaji_to_kanji eki_romaji rest

(* テスト *)
let test_romaji_to_kanji1 =
  romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"

let test_romaji_to_kanji2 = romaji_to_kanji "tokyo" global_ekimei_list = "東京"

let test_romaji_to_kanji3 =
  romaji_to_kanji "shibuya" global_ekimei_list = "渋谷"

let test_romaji_to_kanji4 =
  romaji_to_kanji "nonexistent" global_ekimei_list = ""

let test_romaji_to_kanji5 =
  romaji_to_kanji "ueno"
    [{ kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "銀座線" }]
  = "上野"

let test_romaji_to_kanji6 = romaji_to_kanji "ueno" [] = ""

(* exer10.11 *)
(* 目的：漢字の駅名を二つと駅間リスト (ekikan_t list) を受け取り，駅間リストの中から2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori eki1_kanji eki2_kanji ekikan_lst =
  match ekikan_lst with
  | [] -> infinity
  | { kiten; shuten; kyori } :: rest ->
      if
        (eki1_kanji = kiten && eki2_kanji = shuten)
        || (eki2_kanji = kiten && eki1_kanji = shuten)
      then
        kyori
      else
        get_ekikan_kyori eki1_kanji eki2_kanji rest

(* テスト *)
let test_get_ekikan_kyori0 =
  get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2

let test_get_ekikan_kyori1 =
  get_ekikan_kyori "代々木上原" "代々木公園" global_ekikan_list = 1.0

let test_get_ekikan_kyori2 =
  get_ekikan_kyori "代々木公園" "代々木上原" global_ekikan_list = 1.0

let test_get_ekikan_kyori3 =
  get_ekikan_kyori "茗荷谷" "後楽園" global_ekikan_list = 1.8

let test_get_ekikan_kyori4 =
  get_ekikan_kyori "茅場町" "日本橋" global_ekikan_list = 0.5

let test_get_ekikan_kyori5 =
  get_ekikan_kyori "茗荷谷" "渋谷" global_ekikan_list = infinity

let test_get_ekikan_kyori6 = get_ekikan_kyori "茗荷谷" "後楽園" [] = infinity

(* exer10.12 *)
(* 目的：ローマ字の駅名を二つ取り，その距離を調べて，結果を表示する *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji eki1_romaji eki2_romaji =
  let eki1_kanji = romaji_to_kanji eki1_romaji global_ekimei_list in
  let eki2_kanji = romaji_to_kanji eki2_romaji global_ekimei_list in
  match (eki1_kanji, eki2_kanji) with
  | "", "" -> eki1_romaji ^ " と " ^ eki2_romaji ^ " という駅は存在しません"
  | "", _ -> eki1_romaji ^ " という駅は存在しません"
  | _, "" -> eki2_romaji ^ " という駅は存在しません"
  | _, _ ->
      let eki_kyori =
        get_ekikan_kyori eki1_kanji eki2_kanji global_ekikan_list
      in
      if eki_kyori = infinity then
        eki1_kanji ^ "駅と" ^ eki2_kanji ^ "駅はつながっていません"
      else
        eki1_kanji ^ "駅から" ^ eki2_kanji ^ "駅までは " ^ string_of_float eki_kyori
        ^ "km です"

(* テスト *)
let test_kyori_wo_hyoji0 =
  kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは 1.2km です"

let test_kyori_wo_hyoji1 =
  kyori_wo_hyoji "myogadani" "korakuen" = "茗荷谷駅から後楽園駅までは 1.8km です"

let test_kyori_wo_hyoji2 =
  kyori_wo_hyoji "myogadani" "shibuya" = "茗荷谷駅と渋谷駅はつながっていません"

let test_kyori_wo_hyoji3 =
  kyori_wo_hyoji "unknown" "myogadani" = "unknown という駅は存在しません"

let test_kyori_wo_hyoji4 =
  kyori_wo_hyoji "myogadani" "unknown" = "unknown という駅は存在しません"

let test_kyori_wo_hyoji5 =
  kyori_wo_hyoji "foo" "bar" = "foo と bar という駅は存在しません"

(* exer12.1 *)
(* 駅名，現在の最短距離，現在の最短経路を持つレコード型 *)
type eki_t =
  { namae : string (* 駅名 *)
  ; saitan_kyori : float (*現在の最短距離*)
  ; temae_list : string list (*現在の最短距離*)
  }

(* exer12.2 *)
(* 目的：ekimei_t 型のリストを受け取ったら，その駅名を使って eki_t 型のリストを作る *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let rec make_eki_list ekimei_lst =
  match ekimei_lst with
  | [] -> []
  | { kanji } :: rest ->
      { namae = kanji; saitan_kyori = infinity; temae_list = [] }
      :: make_eki_list rest

(* テスト *)
let test_make_eki_list1 = make_eki_list [] = []

let test_make_eki_list2 =
  make_eki_list
    [ { kanji = "茗荷谷"
      ; kana = "みょうがだに"
      ; romaji = "myogadani"
      ; shozoku = "丸ノ内線"
      } ]
  = [{ namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }]

let test_make_eki_list3 =
  make_eki_list
    [ { kanji = "茗荷谷"
      ; kana = "みょうがだに"
      ; romaji = "myogadani"
      ; shozoku = "丸ノ内線"
      }
    ; { kanji = "新宿"; kana = "しんじゅく"; romaji = "shinjuku"; shozoku = "山手線" }
    ]
  = [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "新宿"; saitan_kyori = infinity; temae_list = [] } ]

(* exer12.3 *)
(* 目的：eki_t 型のリストと起点を受け取ったら，起点を次の状態 (saitan_kyori は 0，temae_list は支店の駅名のみからなるリスト) にする *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika eki_lst start =
  match eki_lst with
  | [] -> []
  | ({ namae } as eki) :: rest ->
      let init_eki =
        if namae = start then
          { namae; saitan_kyori = 0.0; temae_list = [start] }
        else
          eki
      in
      init_eki :: shokika rest start

(* テスト *)
let test_shokika1 = shokika [] "茗荷谷" = []

let test_shokika2 =
  shokika [{ namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }] "茗荷谷"
  = [{ namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"] }]

let test_shokika3 =
  shokika
    [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "新宿"; saitan_kyori = infinity; temae_list = [] } ]
    "新宿"
  = [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "新宿"; saitan_kyori = 0.0; temae_list = ["新宿"] } ]

let test_shokika4 =
  shokika
    [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "新宿"; saitan_kyori = infinity; temae_list = [] } ]
    "渋谷"
  = [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "新宿"; saitan_kyori = infinity; temae_list = [] } ]

(* exer12.4 *)
let rec insert_ekimei ekimei_lst ekimei =
  match ekimei_lst with
  | [] -> [ekimei]
  | ({ kana } as first) :: rest ->
      if kana = ekimei.kana then
        ekimei_lst
        (* insert_ekimei rest ekimei *)
      else if kana < ekimei.kana then
        first :: insert_ekimei rest ekimei
      else
        ekimei :: ekimei_lst

let test_insert_ekimei1 =
  insert_ekimei []
    { kanji = "英"; kana = "えい"; romaji = "ei"; shozoku = "A線" }
  = [{ kanji = "英"; kana = "えい"; romaji = "ei"; shozoku = "A線" }]

let test_insert_ekimei2 =
  insert_ekimei
    [ { kanji = "英"; kana = "えい"; romaji = "ei"; shozoku = "A線" }
    ; { kanji = "美"; kana = "びい"; romaji = "bi"; shozoku = "B線" } ]
    { kanji = ""; kana = "えい"; romaji = ""; shozoku = "" }
  = [ { kanji = "英"; kana = "えい"; romaji = "ei"; shozoku = "A線" }
    ; { kanji = "美"; kana = "びい"; romaji = "bi"; shozoku = "B線" } ]

(* 目的：ekimei_t 型のリストを受け取ったら，ひらがなの順に整列し，さらに駅の重複を取り除いた ekimei_t 型のリストを返す *)
(* seiretsu : ekimei_t -> ekimei_t *)
let rec seiretsu ekimei_lst =
  match ekimei_lst with
  | [] -> []
  | first :: rest -> insert_ekimei (seiretsu rest) first

let test_seiretsu1 = [] = []

let test_seiretsu2 =
  seiretsu
    [ { kanji = "A"; kana = "あ"; romaji = "a"; shozoku = "L1" }
    ; { kanji = "B"; kana = "あ"; romaji = "a"; shozoku = "L2" } ]
  = [{ kanji = "B"; kana = "あ"; romaji = "a"; shozoku = "L2" }]

let test_seiretsu3 =
  seiretsu
    [ { kanji = "美"; kana = "びい"; romaji = "bi"; shozoku = "B線" }
    ; { kanji = "英"; kana = "えい"; romaji = "ei"; shozoku = "A線" }
    ; { kanji = "栄"; kana = "えい"; romaji = "ei"; shozoku = "A線" }
    ; { kanji = "衛"; kana = "えい"; romaji = "ei"; shozoku = "A線" } ]
  = [ { kanji = "衛"; kana = "えい"; romaji = "ei"; shozoku = "A線" }
    ; { kanji = "美"; kana = "びい"; romaji = "bi"; shozoku = "B線" } ]

let test_seiretsu4 =
  seiretsu [{ kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" }]
  = [{ kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" }]

let test_seiretsu5 =
  seiretsu
    [ { kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" }
    ; { kanji = "赤"; kana = "あか"; romaji = "aka"; shozoku = "テスト線" } ]
  = [ { kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" }
    ; { kanji = "赤"; kana = "あか"; romaji = "aka"; shozoku = "テスト線" } ]

let test_seiretsu6 =
  seiretsu
    [ { kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" }
    ; { kanji = "赤"; kana = "あか"; romaji = "aka"; shozoku = "テスト線" }
    ; { kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" } ]
  = [ { kanji = "青"; kana = "あお"; romaji = "ao"; shozoku = "テスト線" }
    ; { kanji = "赤"; kana = "あか"; romaji = "aka"; shozoku = "テスト線" } ]

let seiretsu_global_ekimei_list_length =
  List.length (seiretsu global_ekimei_list)

(* exer13.6 *)
(* 目的：直前に確定した駅 (eki_t 型) と未確定の駅 q (eki_t 型) を受け取ったら， p と q 直接つながっているかどうかを調べ，
   つながっていたら q の最短距離と手前リストを必要に応じて更新したもの - つながっていなかったら元の q をそのまま返す関数 *)
(* koushin1 : eki_t -> eki_t -> eki_t *)
let koushin1_ekikan ekikan_lst p q =
  let pq_kyori = get_ekikan_kyori p.namae q.namae ekikan_lst in
  let is_connected = pq_kyori <> infinity in
  (* お行儀よく *)
  let new_kyori = p.saitan_kyori +. pq_kyori in
  if is_connected && new_kyori < q.saitan_kyori then
    { namae = q.namae
    ; saitan_kyori = new_kyori
    ; temae_list = q.namae :: p.temae_list
    }
  else
    (* p-q が繋がっていない or 距離が小さくならない場合 *)
    q

let koushin1 p q = koushin1_ekikan global_ekikan_list p q

let test_koushin1 =
  (* 直結していて、更新した方が短くなるケース *)
  let p = { namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"] } in
  let q = { namae = "代々木公園"; saitan_kyori = infinity; temae_list = [] } in
  koushin1 p q

let test_koushin1_1 =
  (* 直結していて、更新した方が短くなるケース *)
  let p = { namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"] } in
  let q = { namae = "代々木公園"; saitan_kyori = infinity; temae_list = [] } in
  koushin1 p q
  = { namae = "代々木公園"; saitan_kyori = 1.0; temae_list = ["代々木公園"; "代々木上原"] }

let test_koushin1_2 =
  (* 直結しているが、すでに q の方が短いので更新しないケース *)
  let p = { namae = "代々木上原"; saitan_kyori = 10.; temae_list = ["代々木上原"] } in
  let q =
    { namae = "代々木公園"; saitan_kyori = 2.; temae_list = ["代々木公園"; "千代田"] }
  in
  koushin1 p q = q

let test_koushin1_3 =
  (* 直結していないのでそのまま返すケース *)
  let p = { namae = "茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"] } in
  let q = { namae = "渋谷"; saitan_kyori = infinity; temae_list = [] } in
  koushin1 p q = q

let test_koushin1_4 =
  (* 直結していても新しい距離が同じなら更新しない（境界値） *)
  let p = { namae = "代々木上原"; saitan_kyori = 1.; temae_list = ["代々木上原"] } in
  let q =
    { namae = "代々木公園"; saitan_kyori = 2.; temae_list = ["代々木公園"; "既存"] }
  in
  koushin1 p q = q

let test_koushin1_5 =
  (* p の最短距離が infinity なら、直結でも更新しない *)
  let p =
    { namae = "代々木上原"; saitan_kyori = infinity; temae_list = ["代々木上原"] }
  in
  let q = { namae = "代々木公園"; saitan_kyori = infinity; temae_list = [] } in
  koushin1 p q = q

let test_koushin1_6 =
  (* 更新時に temae_list が q.namae :: p.temae_list になることを確認 *)
  let p =
    { namae = "代々木上原"; saitan_kyori = 3.; temae_list = ["代々木上原"; "A"; "B"] }
  in
  let q = { namae = "代々木公園"; saitan_kyori = infinity; temae_list = [] } in
  koushin1 p q
  = { namae = "代々木公園"
    ; saitan_kyori = 4.0
    ; temae_list = ["代々木公園"; "代々木上原"; "A"; "B"]
    }

(* exer13.7 *)
(* 目的：直前に確定した駅 (eki_t 型) と未確定の駅のリスト v (eki_t list 型) を受け取ったら，
   必要な更新処理を行った後の未確定の駅のリストを返す関数 *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v = List.map (koushin1 p) v

(* テスト *)
let test_ekikan_list =
  [ { kiten = "A"; shuten = "B"; keiyu = "Book"; kyori = 10.0; jikan = 0 }
  ; { kiten = "A"; shuten = "D"; keiyu = "Book"; kyori = 4.0; jikan = 0 }
  ; { kiten = "B"; shuten = "C"; keiyu = "Book"; kyori = 2.0; jikan = 0 }
  ; { kiten = "B"; shuten = "E"; keiyu = "Book"; kyori = 2.0; jikan = 0 }
  ; { kiten = "C"; shuten = "E"; keiyu = "Book"; kyori = 1.0; jikan = 0 }
  ; { kiten = "D"; shuten = "E"; keiyu = "Book"; kyori = 3.0; jikan = 0 } ]

(* テストしにくいので，koushin の ekikan_lst を取る版 *)
let koushin_ekikan ekikan_lst p v = List.map (koushin1_ekikan ekikan_lst p) v

(* fig12.2 -> fig12.3 *)
let test1_koushin =
  let p = { namae = "A"; saitan_kyori = 0.0; temae_list = ["A"] } in
  let v =
    [ { namae = "B"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "C"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "D"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "E"; saitan_kyori = infinity; temae_list = [] } ]
  in
  koushin_ekikan test_ekikan_list p v
  = [ { namae = "B"; saitan_kyori = 10.0; temae_list = ["B"; "A"] }
    ; { namae = "C"; saitan_kyori = infinity; temae_list = [] }
    ; { namae = "D"; saitan_kyori = 4.0; temae_list = ["D"; "A"] }
    ; { namae = "E"; saitan_kyori = infinity; temae_list = [] } ]
