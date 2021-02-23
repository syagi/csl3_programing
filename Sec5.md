# 5.1
(1) int

(2) float

(3) bool

(4) Error "2"はintでないとダメ


# 5.2
```
(* 目的: 午前か午後を返す *)
(* jikan : int -> string *)
let jikan t =
  if t mod 24 < 12 then "gozen" else "gogo" ;;

(* test *)
let test1 = jikan 0 = "gozen";;
let test2 = jikan 11 = "gozen";;
let test3 = jikan 12 = "gogo";;
let test4 = jikan 23 = "gogo";;
let test5 = jikan 24 = "gozen";;
```

実行結果
```

let test1 = jikan 0 = "gozen";;
val test1 : bool = true
let test2 = jikan 11 = "gozen";;
val test2 : bool = true
let test3 = jikan 12 = "gogo";;
val test3 : bool = true
let test4 = jikan 23 = "gogo";;
val test4 : bool = true
let test5 = jikan 24 = "gozen";;
val test5 : bool = true
```

# 5.3
```
(* 目的: 星座を返す *)
(* seiza : int -> int -> string *)
let seiza m d =
  if      ( m=3 && d>20 ) || ( m=4 && d<20 ) then "Aries"
  else if ( m=4 && d>19) || (m=5 && d<21) then "Taurs"
  else if ( m=5 && d>20) || (m=6 && d<22) then "Gemini"
  else "残りは省略" ;;
(* test *)
let test1 = seiza 4 19 = "Aries";;
let test2 = seiza 4 20 = "Taurs";;
let test3 = seiza 5 20 = "Taurs";;
let test4 = seiza 5 21 = "Gemini";;

```

実行結果
```
let test1 = seiza 4 19 = "Aries";;
val test1 : bool = true
let test2 = seiza 4 20 = "Taurs";;
val test2 : bool = true
let test3 = seiza 5 20 = "Taurs";;
val test3 : bool = true
let test4 = seiza 5 21 = "Gemini";;
val test4 : bool = true
```

# 5.4

```
(* 目的：判別式を計算 *)
(* hanbets : float -> float -> float -> float *)

let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c ;;

(* test *)
let test1 = hanbetsushiki 2. 3. (-1.) = 17. ;;

```

# 5.5

```
(* 目的：解の個数 *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0. then 2
  else if hanbetsushiki a b c = 0. then 1
  else 0 ;;

(* test *)
let test1 = kai_no_kosuu 1. 2. 1. = 1;;
let test2 = kai_no_kosuu 1. 2. (-1.) = 2;;
let test3 = kai_no_kosuu 1. 2. 2. = 0;;

```

# 5.6

```
(* 目的：虚数解の有無 *)
(* kyosuukai : float -> float -> float -> bool *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0. then true
  else false ;;

(* test *)
let test1 = kai_no_kosuu 1. 2. 1. = false;;
let test2 = kai_no_kosuu 1. 2. (-1.) = true ;;
let test3 = kai_no_kosuu 1. 2. 2. = false;;

```

# 5.7

```
(* 目的：体型を返す *)
(* taikei: float -> float -> string *)
let bmi h w = w /. h ** 2. ;;
let taikei h w =
  if bmi h w < 18.5 then "Yase"
  else if bmi h w < 25. then "Hyojun"
  else if bmi h w < 30. then "Himan"
  else "Koudo Himan";;

(* test *)
let test1 = taikei 1.6 40. = "Yase";;
let test2 = taikei 1.6 60. = "Hyojun";;
let test3 = taikei 1.6 70. = "Himan";;
let test4 = taikei 1.6 80. = "Koudo Himan";;
```

# 6.1
```
let square x=x*x;;
square 3. ;;

let circle = 2. +. pi +. r ;;
circle 2.;;

let TV bangumi youbi =
  bangumi ^ "は" ^ youbi ^ "に放映";;
TV "ポケモン" "木曜日" ;;
```
実行結果

```
let square x=x*x;;
val square : float -> float = <fun>
# square 3. ;;
Error: This expression has type float but an expression was expected of type
       float

# let circle = 2. +. pi +. r ;;
Error: Unbound value pi

# circle 2.;;
Error: Unbound value circle

# let TV bangumi youbi =
  bangumi ^ "は" ^ youbi ^ "に放映";;
Error: Syntax error

# TV "ポケモン" "木曜日" ;;
Error: Syntax error

```

# 7.1
```
let goukei_to_heikin k m e r s = ( k, m, e, r, s, (k+m+e+r+s)/5);;

(* test *)
let test1 = goukei_to_heikin 90 80 50 20 10 = (90, 80, 50, 20, 10, 50);;
```

# 7.2

```
let seiseki pair = match pair
  with (name, score) -> name ^ "の成績は" ^ score;;

let test1 = seiseki ("やぎ", "C") = "やぎの成績はC";;

```

# 7.3
```
(* chuuten: (float, float) -> (float, float) *)
let taisho_x zaho = match zaho with (x,y) -> (-1. *. x, y);;

let test1 = taisho_x (1. , 1.) = (-1. , 1. );;

```

# 7.4
```
(* chuuten: (float, float) -> (float, float) -> (float, float) *)

let chuuten p1 p2 =
  match p1 with (x1, y1) ->
    match p2 with (x2, y2) ->
      ( (x1+.x2)/.2., (y1+.y2)/.2. );;

let test1 = chuuten (1.,1.) (-1.,-1.) = (0.,0;;

```

# 8.1

```

type book_t = {
 title: string;
 author: string;
 isbn: string;
};;

let pg = {title = "pg入門"; author = "asai"; isbn="1160"};;
let db = {title = "DragonBall"; author= "toriyama"; isbn="0000"};;

```

# 8.2

```

type okozukai_t = {
  name: string;
  price: int;
  shop: string;
  date: string;
};;

let p1 = {name="米"; price=1500; shop="米屋"; date="2021/2/20"};;
let p2 = {name="味噌"; price=500; shop="味噌屋"; date="2021/2/20"};;

```

# 8.3
```
type person_t = {
  name: string;
  hight: float;
  weight: float;
  birthday: string ;
  blood: string;
};;

let p1 = {name="yagi"; hight=160.; weight=60.; birthday="6/18"; blood="A" };;

```

# 8.4
```
(* ketsuekigata_hyoji person_t -> string *)
let ketsuekigata_hyoji person =
   match person with
     {name=n; hight=h; weight=w; birthday=bd; blood=b } ->
     n^"の血液型は"^b ;;

(* test *)
let p1 = {name="yagi"; hight=160.; weight=60.; birthday="6/18"; blood="A" };;
let test1= ketsuekigata_hyoji p1 = "yagiの血液型はA";;

```

# 8.5
```
type ekimei_t ={
 kanji: string;
 kana: string;
 romaji: string;
 shozoku: string
};;
```

# 8.6
```
let hyoji eki = match eki with
  { kanji=kanji; kana=kana; romaji=roma;shozoku=s}
  -> s ^ ", " ^ kanji ^ "(" ^ kana ^ ")";;

let tokyo =
  {kanji="東京"; kana="とうきょう"; romaji="tokyo"; shozoku="中央線"};;
let test1 = hyoji tokyo = "中央線, 東京(とうきょう)";;

```

# 8.7
```
type ekikan_t ={
  kiten: string;
  shuten: string;
  keiyu: string;
  kyori: float;
  jikan: int;
}
```

# 9.1
```
"春" :: "夏" :: "秋" :: "冬" :: [] ;;
- : string list = ["春"; "夏"; "秋"; "冬"]
```

# 9.2
```
let p1 = {name="yagi1"; hight=160.; weight=60.; birthday="6/18"; blood="A" };;
let p2 = {name="yagi2"; hight=161.; weight=61.; birthday="6/19"; blood="B" };;
let p3 = {name="yagi3"; hight=162.; weight=62.; birthday="6/20"; blood="C" };;

p1 :: p2 :: p3 :: [] ;;
```

```
- : person_t list =
[{name = "yagi1"; hight = 160.; weight = 60.; birthday = "6/18"; blood = "A"};
 {name = "yagi2"; hight = 161.; weight = 61.; birthday = "6/19"; blood = "B"};
 {name = "yagi3"; hight = 162.; weight = 62.; birthday = "6/20"; blood = "C"}]
 ```

# 9.3
 ```
 ["春"; "夏"; "秋"; "冬"];;
 ```

# 9.4
```
let rec length lst = match lst with
  [] -> 0
  | first::rest -> 1 + length rest;;

let test1 = length [0;1;2]=3;;

```

# 9.5
```
let rec even lst = match lst with
  [] -> []
  | first :: rest
   -> if first mod 2 = 0
      then first :: even rest
      else even rest;;

let test1 = even [2; 1; 6; 4; 7] = [2;6;4];;

```

# 9.6

```
let rec concat lst = match lst with
   [] -> ""
   | first :: rest -> first ^ concat rest ;;

let test1 = concat  ["春"; "夏"; "秋"; "冬"] = "春夏秋冬";;

```

# 9.7
```
type person_t = {
  name: string;
  hight: float;
  weight: float;
  birthday: string ;
  blood: string;
};;

let rec count_ketsueki_A lst =
  match lst with
  [] -> 0
  | {name=n; hight=h; weight=w; birthday=bd; blood=b } :: rest
    -> (if b="A" then 1 else 0) + count_ketsueki_A rest;;

let plist = [{name = "yagi1"; hight = 160.; weight = 60.; birthday = "6/18"; blood = "A"};
 {name = "yagi2"; hight = 161.; weight = 61.; birthday = "6/19"; blood = "B"};
 {name = "yagi3"; hight = 162.; weight = 62.; birthday = "6/20"; blood = "A"}];;
 let test1 = count_ketsueki_A plist = 2;;

```

# 9.8
```
type date_t = {month:int; day:int};;
type person_t = {
  name: string;
  hight: float;
  weight: float;
  birthday: date_t ;
  blood: string;
};;

let rec otomeza lst =
  match lst with
  [] -> []
  | {name=n; hight=h; weight=w; birthday={month=m; day=d}; blood=b } :: rest
    -> if (m=8 && d>22) || (m=9 && d<23)
         then n :: otomeza rest
         else otomeza rest;;

let plist = [{name = "yagi1"; hight = 160.; weight = 60.; birthday = {month=6;day=10}; blood = "A"};
 {name = "yagi2"; hight = 161.; weight = 61.; birthday = {month=9;day=22}; blood = "B"};
 {name = "yagi3"; hight = 162.; weight = 62.; birthday = {month=8;day=30}; blood = "A"}];;
 let test1 = otomeza plist = ["yagi2" ; "yagi3"];;

```
