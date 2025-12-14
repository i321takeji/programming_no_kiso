(* exer 8.1 *)
(* 目的：本のタイトル，著者名，出版社，値段，ISBN を持つレコード型 *)
type book_t =
  {title: string; author: string; publisher: string; price: int; isbn: string}

let ex1book =
  { title= "programming no kiso"
  ; author= "asai"
  ; publisher= "science sya"
  ; price= 2300
  ; isbn= "978-4-7819-1160-1" }

let ex2book =
  { title= "Java no ehon"
  ; author= "taro"
  ; publisher= "prog1"
  ; price= 1300
  ; isbn= "123-4-5678-9012-3" }

let ex3book =
  { title= "Haskell no hon"
  ; author= "jiro"
  ; publisher= "prog2"
  ; price= 3300
  ; isbn= "987-6-5432-1012-3" }

(* exer 8.2 *)
(* 目的：買ったものの名前，値段，買った場所，日付を持つレコード型 *)
type okozukai_t = {name: string; price: int; place: string; date: string}

let ex1okozukai = {name= "taro"; price= 1000; place= "Nagoya"; date= "11/23"}

let ex2okozukai = {name= "jiro"; price= 1500; place= "Gifu"; date= "10/30"}

let ex3okozukai = {name= "saburo"; price= 2500; place= "Mie"; date= "12/05"}

(* exer 8.3 *)
(* 人と名前，身長 (m)，体重 (kg)，誕生日 (月と日)，血液型を表す型 *)
type person_t =
  { name: string
  ; height: float
  ; weight: float
  ; birth: int * int
  ; blood: string }

let ex1person =
  {name= "taro"; height= 1.7; weight= 65.0; birth= (2, 10); blood= "A"}

let ex2person =
  {name= "jaro"; height= 1.6; weight= 75.3; birth= (3, 9); blood= "B"}

let ex3person =
  {name= "saburo"; height= 1.8; weight= 55.1; birth= (10, 23); blood= "O"}

(* exer 8.4 *)
(* 目的：person_t 型のデータを受け取り「○○さんの血液型は△型です」という文字列を返す *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyoji person =
  match person with
  | {name; blood} ->
      (* { name; height; weight; birth; blood } -> *)
      name ^ "さんの血液型は" ^ blood ^ "型です"

(* テスト *)
let test1 =
  ketsueki_hyoji
    {name= "yamada"; height= 170.0; weight= 60.0; birth= (1, 1); blood= "A"}
  = "yamadaさんの血液型はA型です"

let test2 =
  ketsueki_hyoji
    {name= "suzuki"; height= 165.0; weight= 55.0; birth= (2, 3); blood= "B"}
  = "suzukiさんの血液型はB型です"

let test3 =
  ketsueki_hyoji
    {name= "tanaka"; height= 180.0; weight= 72.0; birth= (6, 21); blood= "O"}
  = "tanakaさんの血液型はO型です"

let test4 =
  ketsueki_hyoji
    {name= "sato"; height= 158.0; weight= 47.0; birth= (12, 12); blood= "AB"}
  = "satoさんの血液型はAB型です"
