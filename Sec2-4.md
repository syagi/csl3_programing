# 2.1
```
# 7 - 3 + 4 ;;
- : int = 8
# 7 / 2 * 2 ;;
- : int = 6
# 7 * 2 / 2 ;;
- : int = 7
```
int型のため、 7/2は丸められる

# 2.2
```
# 2. *. 3.14 *. 10. ;;
- : float = 62.8000000000000043
# 1.73 ** 2. ;;
- : float = 2.9929
# 7. /. 2. ;;
- : float = 3.5
```

# 2.3
```
# "東京" ^ "特許" ^ "許可局" ^ "局長" ;;
- : string = "東京特許許可局局長"
# "関数" ^ "型" ^ "言語" ;;
- : string = "関数型言語"
```

# 2.4
```
# 2 > 3 ;;
- : bool = false
# not  ( 3.1415 ** 2. > 10. ) ;;
- : bool = true
# 8 mod 3 = 2 ;;
- : bool = true
# 3 + 4 + 5 = 4 * 3 ;;
- : bool = true
```

# 3.1
e : float  
positive : bool  
seconds_of_day : int  
name : string  

```
# let e = 2.7182 ;;
val e : float = 2.7182
# let positive = e > 0. ;;
val positive : bool = true
# let seconds_of_day = 60*60*24 ;;
val seconds_of_day : int = 86400
# let name = "茗荷谷" ;;
val name : string = "茗荷谷"
```

# 3.2
```
# 1.0 +. e *. 2.0 ;;
- : float = 6.4364
```

# 4.1
```
# let baito_kyryo y h = (850 + 100 * y) * h;;
val baito_kyryo : int -> int -> int = <fun>
# baito_kyryo 0 1;;
- : int = 850
# baito_kyryo 1 1;;
- : int = 950
# baito_kyryo 2 1;;
- : int = 1050
```

# 4.2
```
# let jikoshokai name = "Hi. This is " ^ name ;;
val jikoshokai : string -> string = <fun>
# jikoshokai "yagi" ;;
- : string = "Hi. This is yagi"
```

# 4.3
```
# let hyojun_taiju h = h ** 2. *. 22. ;;
val hyojun_taiju : float -> float = <fun>
# hyojun_taiju 160. ;;
- : float = 563200.
```

# 4.4
```
# let bmi h w = w /. h ** 2. ;;
val bmi : float -> float -> float = <fun>
# bmi 1.60 60. ;;
- : float = 23.4374999999999964
```

# 4.5
4.1〜4.4の実行結果に書いてあるとおり

# 4.6
## (1) (2)
```
(* 鶴の足を求める。*)
(* tsuru_no_ashi : int -> int *)
```

## (3) (4)
```
let test1 = tsuru_no_ashi 1 = 2 ;;
let test2 = tsuru_no_ashi 2 = 4 ;;
let test3 = tsuru_no_ashi 0 = 0 ;;
```

## (5)
```
tsuru_no_ashi n = n * 2 ;;
```

## (6)
```
# let tsuru_no_ashi n = n * 2 ;;
val tsuru_no_ashi : int -> int = <fun>
```

## (7)
```
# let test1 = tsuru_no_ashi 1 = 2 ;;
let test2 = tsuru_no_ashi 2 = 4 ;;
let test3 = tsuru_no_ashi 0 = 0 ;;
val test1 : bool = true
# val test2 : bool = true
# val test3 : bool = true
```

# 4.7
## (1) (2)
```
(* 鶴と亀の足を求める。*)
(* tsurukame_no_ashi : int -> int -> int *)
```

## (3) (4)
```
let test1 = tsurukame_no_ashi 1 1 = 6 ;;
let test2 = tsurukame_no_ashi 1 0 = 2 ;;
let test3 = tsurukame_no_ashi 0 1 = 4 ;;
```

## (5)
```
tsurukame_no_ashi t k  = n * 2 + k * 4 ;;
```

## (6)
```
# let tsurukame_no_ashi t k = t * 2 + k *4 ;;
val tsurukame_no_ashi : int -> int -> int = <fun>
```

## (7)
```
# let test1 = tsurukame_no_ashi 1 1 = 6 ;;
let test2 = tsurukame_no_ashi 1 0 = 2 ;;
let test3 = tsurukame_no_ashi 0 1 = 4 ;;
val test1 : bool = true
# val test2 : bool = true
# val test3 : bool = true
```

# 4.8
## (1) (2)
```
(* 足の数から鶴を求める。*)
(* tsurukame : int -> int -> int *)
```

## (3) (4)
```
let test1 = tsurukame 1 2 = 1 ;;
let test2 = tsurukame 1 4 = 0 ;;
let test3 = tsurukame 2 6 = 1 ;;
```

## (5)
```
tsurukame n f  = (4 * n - f ) / 2 ;;
```
## (6)
```
# let tsurukame n f  = (4 * n - f ) / 2 ;;
val tsurukame : int -> int -> int = <fun>
```

## (7)
```
# let test1 = tsurukame 1 2 = 1 ;;
let test2 = tsurukame 1 4 = 0 ;;
let test3 = tsurukame 2 6 = 1 ;;
# val test1 : bool = true
val test2 : bool = true
val test3 : bool = true
```
