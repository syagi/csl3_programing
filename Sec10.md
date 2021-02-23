# 10.1

```
let rec insert lst num = match lst with
  [] -> num :: []
  | first :: rest
      -> if first > num
         then num :: first :: rest
         else first :: insert rest num ;;

let test1 = insert [1; 3; 4; 7; 8] 5 = [1;3;4;5;7;8];;
let test2 = insert [] 1 = [1];;
```

# 10.2
```
let rec ins_sort lst = match lst with
  [] -> []
  | first :: rest -> insert (ins_sort rest) first;;

let test1 = ins_sort [5;3;8;1;7;4] = [1;3;4;5;7;8];;

```

# 10.3
```
type gakusei_t = {
  name: string;
  tensuu: int;
  seiseki: string
};;

let rec gakusei_ins lst g = match g with
  { name=gn; tensuu=gt; seiseki=gs } -> match lst with
    [] -> g :: []
    | { name=fn; tensuu=ft; seiseki=fs } :: rest ->
       if ft > gt
        then { name=gn; tensuu=gt; seiseki=gs } :: { name=fn; tensuu=ft; seiseki=fs } :: rest
        else { name=fn; tensuu=ft; seiseki=fs } :: gakusei_ins rest g ;;

let rec gakusei_sort lst = match lst with
  [] -> []
  | first :: rest -> gakusei_ins (gakusei_sort rest) first ;;


(* test of gakusei_ins *)
let g1 = {name="a"; tensuu=10; seiseki="C"};;
let g2 = {name="b"; tensuu=20; seiseki="C"};;
let g3 = {name="c"; tensuu=30; seiseki="B"};;
let g4 = {name="d"; tensuu=40; seiseki="A"};;
let glist= [g1;g3;g4];;

let test1 = gakusei_ins glist g2 = [g1; g2; g3; g4];;

(* test of gakusei_sort *)
let glist2= [g3; g2; g4; g1];;

let test1 = gakusei_sort glist2 = [g1; g2; g3; g4];;

```

# 10.4
```
type person_t = {
  name: string;
  hight: float;
  weight: float;
  birthday: string ;
  blood: string;
};;

let rec person_ins lst p = match p with
  {name=n; hight=h; weight=w; birthday=bd; blood=b } -> match lst with
    [] -> p :: []
    | {name=fn; hight=fh; weight=fw; birthday=fbd; blood=fb } :: rest ->
       if fn > n
        then {name=n; hight=h; weight=w; birthday=bd; blood=b }
             :: {name=fn; hight=fh; weight=fw; birthday=fbd; blood=fb } :: rest
        else {name=fn; hight=fh; weight=fw; birthday=fbd; blood=fb }
             :: person_ins rest p ;;

let rec person_sort lst = match lst with
    [] -> []
    | first :: rest -> person_ins (person_sort rest) first ;;

(* test *)
let p1 = {name="agi"; hight=160.; weight=60.; birthday="6/18"; blood="A" };;
let p2 = {name="kagi"; hight=161.; weight=61.; birthday="6/19"; blood="B" };;
let p3 = {name="sagi"; hight=162.; weight=62.; birthday="6/20"; blood="C" };;
let p4 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="C" };;
let plist = [p3; p2; p4; p1;];;
let test1 = person_sort plist = [p1; p2; p3; p4];;

```

# 10.5
```
type gakusei_t = {
  name: string;
  tensuu: int;
  seiseki: string
};;

let gakusei_better a b =
  match a with { name=an; tensuu=at; seiseki=asei }
  -> match b with { name=bn; tensuu=bt; seiseki=bsei; }
     -> if at > bt then a else b ;;

let rec gakusei_max lst = match lst with
   [] -> { name="nobody"; tensuu = -1; seiseki="Z" }
   | first :: rest -> gakusei_better first (gakusei_max rest)  ;;

(*test*)
let g1 = {name="a"; tensuu=10; seiseki="C"};;
let g2 = {name="b"; tensuu=20; seiseki="C"};;
let g3 = {name="c"; tensuu=30; seiseki="B"};;
let g4 = {name="d"; tensuu=40; seiseki="A"};;
let glist= [g1;g3;g4;g2];;

let test1 = gakusei_max glist =  g4 ;;
```

# 10.7
```
type person_t = {
  name: string;
  hight: float;
  weight: float;
  birthday: string ;
  blood: string;
};;

let rec ketsueki_shukei lst = match lst with
   [] -> (0, 0, 0, 0)
   | {name=n; hight=h; weight=w; birthday=bd; blood=bt } :: rest
     -> let shukei_rest = ketsueki_shukei rest in
         match shukei_rest with (a, b, o, ab)
           -> if bt="A" then (a+1, b, o, ab)
              else if bt="B" then (a, b+1, o, ab)
              else if bt="O" then (a, b, o+1, ab)
              else (a, b, o, ab+1);;

(*test*)
let p1 = {name="agi"; hight=160.; weight=60.; birthday="6/18"; blood="A" };;
let p2 = {name="kagi"; hight=161.; weight=61.; birthday="6/19"; blood="B" };;
let p3 = {name="sagi"; hight=162.; weight=62.; birthday="6/20"; blood="O" };;
let p4 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="AB" };;
let p5 = {name="kagi"; hight=161.; weight=61.; birthday="6/19"; blood="B" };;
let p6 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="AB" };;
let p7 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="AB" };;
let plist = [p3; p2; p4; p1; p5; p6; p7];;
let test1 = ketsueki_shukei plist = (1,2,1,3);;

```

# 10.8

```
let rec get_max lst = match lst with
  [] -> 0
  | first::rest -> let rest_max = get_max rest in
    if first > rest_max then first else rest_max ;;

let saita_ketsueki lst =
  let shukei = ketsueki_shukei lst in
    match shukei with (a, b, o, ab)
      -> let max = get_max [a;b;o;ab] in
        if a = max then "A"
        else if b = max then "B"
        else if o = max then "O"
        else "AB";;

(*test*)
let p1 = {name="agi"; hight=160.; weight=60.; birthday="6/18"; blood="A" };;
let p2 = {name="kagi"; hight=161.; weight=61.; birthday="6/19"; blood="B" };;
let p3 = {name="sagi"; hight=162.; weight=62.; birthday="6/20"; blood="O" };;
let p4 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="AB" };;
let p5 = {name="kagi"; hight=161.; weight=61.; birthday="6/19"; blood="B" };;
let p6 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="AB" };;
let p7 = {name="tagi"; hight=162.; weight=62.; birthday="6/20"; blood="AB" };;
let plist = [p3; p2; p4; p1; p5; p6; p7];;
let test1 = saita_ketsueki plist = "AB";;

```


# 10.9
```
let rec equal_length a b = match (a,b) with
  ([],[]) -> "equal length"
  | ([], f2 :: r2 ) -> "b is longer"
  | (f1 :: r1 , [] ) -> "a is longer"
  | (f1 :: r1 , f2 :: r2 ) -> equal_length r1 r2 ;;

(* test *)
let lst1=[1;2;3];;
let lst2=[1;2];;
let test1 = equal_length lst1 lst2 = "a is longer";;
let test2 = equal_length lst2 lst1 = "b is longer";;
let test3 = equal_length lst1 lst1 = "equal length";;

```

# 10.10
```
let rec romaji_to_kanji eki lst = match lst with
  [] -> eki ^ " not found"
  | { kanji=kanji; kana=kana; romaji=roma;shozoku=s} :: rest
    -> if eki=roma then kanji else romaji_to_kanji eki rest ;;

(*test*)
let s1 = {kanji="東京"; kana="とうきょう"; romaji="tokyo"; shozoku="中央線"};;
let s2 = {kanji="神田"; kana="かんだ"; romaji="kanda"; shozoku="中央線"};;
let s3 = {kanji="四ッ谷"; kana="よつや"; romaji="yotsuya"; shozoku="中央線"};;
let slist = [s1;s2;s3];;
let test1 = romaji_to_kanji "kanda" slist = "神田";;
let test2 = romaji_to_kanji "ochanomizu" slist = "ochanomizu not found";;

```

# 10.11
```
type ekikan_t ={
  kiten: string;
  shuten: string;
  keiyu: string;
  kyori: float;
  jikan: int;
};;

let rec ekikan_kyori s1 s2 lst = match lst with
  [] -> infinity
  | {
    kiten=kiten;
    shuten=shuten;
    keiyu=keyu;
    kyori=kyori;
    jikan=jikan;
  } :: rest
   -> if (s1=kiten && s2=shuten) || (s2=kiten && s1=shuten)
       then kyori else ekikan_kyori s1 s2 rest ;;

(*test*)
let d1 = {kiten="西船橋"; shuten="原木中山"; keiyu="東西線"; kyori=1.9; jikan=3};;
let d2 = {kiten="原木中山"; shuten="妙典"; keiyu="東西線"; kyori=2.1; jikan=2};;
let dlist = [d1;d2];;
let test1 = ekikan_kyori "西船橋" "原木中山" dlist = 1.9;;
let test2 = ekikan_kyori "妙典" "原木中山" dlist = 2.1;;
let test3 = ekikan_kyori "妙典" "西船橋" dlist = infinity;;

```

# 10.12
```
let kyori_wo_hyoji rs1 rs2 ekimei ekikan =
   let ks1=romaji_to_kanji rs1 ekimei in
     if ks1 = rs1^" not found" then rs1 ^ " という駅は存在しません"
     else let ks2=romaji_to_kanji rs2 ekimei in
       if ks2 = rs2^" not found" then rs2 ^ " という駅は存在しません"
       else let dist=ekikan_kyori ks1 ks2 ekikan in
         if dist=infinity then ks1 ^" と "^ ks2 ^" はつながっていません"
         else ks1 ^ " から " ^ ks2 ^" までは "^ (string_of_float dist) ^ "kmです";;

(* test *)
let s1 = {kanji="西船橋"; kana="にしふなばし"; romaji="nishi-funabashi"; shozoku="東西線"};;
let s2 = {kanji="原木中山"; kana="ばらきなかやま"; romaji="baraki-nakayama"; shozoku="東西線"};;
let s3 = {kanji="妙典"; kana="みょうでん"; romaji="myoden"; shozoku="東西線"};;
let slist = [s1;s2;s3];;

let d1 = {kiten="西船橋"; shuten="原木中山"; keiyu="東西線"; kyori=1.9; jikan=3};;
let d2 = {kiten="原木中山"; shuten="妙典"; keiyu="東西線"; kyori=2.1; jikan=2};;
let dlist = [d1;d2];;

let test1 = kyori_wo_hyoji "nishi-funabashi" "baraki-nakayama" slist dlist
  = "西船橋 から 原木中山 までは 1.9kmです";;
let test2 = kyori_wo_hyoji "nishi-funabashi" "myoden" slist dlist
  = "西船橋 と 妙典 はつながっていません";;
let test3 = kyori_wo_hyoji "minami-funabashi" "baraki-nakayama" slist dlist
  = "minami-funabashi という駅は存在しません";;
```

# 11.1
```
let rec power m n =
  if n = 0 then 1
  else m * power m (n-1) ;;

let rec sum_of_square n =
  if n = 0 then 0
  else (power n 2) + sum_of_square (n-1);;

let test1 = sum_of_square 4 = 30;;
let test2 = sum_of_square 0 = 0;;

```

# 11.2
```
let rec a n =
  if n=0 then 3
  else -1 + 2 * a (n-1) ;;

let test1 = a 0 = 3;;
let test2 = a 1 = 5;;
let test3 = a 2 = 9;;

```
