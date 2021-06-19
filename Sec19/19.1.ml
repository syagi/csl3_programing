(* global_ekikan_list, global_ekimei_list *)
#use "metro.ml"

let rec ekimei_insert lst ekimei0 = match lst with
    [] -> [ekimei0]
  | ({kanji = k; kana = a; romaji = r; shozoku = s} as ekimei) :: rest ->
      match ekimei0 with {kanji = k0; kana = a0; romaji = r0; shozoku = s0} ->
      	if a = a0 then ekimei_insert rest ekimei0
	      else if a < a0 then ekimei :: ekimei_insert rest ekimei0
	      else ekimei0 :: lst

let make_initial_eki_list ekimei_list kiten =
  List.map (fun ekimei -> match ekimei with
	     {kanji = k; kana = a; romaji = r; shozoku = s} ->
	       if k = kiten
	       then {namae = k; saitan_kyori = 0.; temae_list = [k]}
	       else {namae = k; saitan_kyori = infinity; temae_list = []})
	   ekimei_list

let rec seiretsu ekimei_list = match ekimei_list with
    [] -> []
  | first :: rest -> ekimei_insert (seiretsu rest) first

let rec romaji_to_kanji r0 ekimei_list = match ekimei_list with
  [] -> ""
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest ->
    if r0 = r then k
    else romaji_to_kanji r0 rest

(* 17.10 *)
type ekikan_tree_t = Empty
| Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t ;;

(* 17.11 *)
(* assoc eki_t -> list -> float *)
let rec assoc dest list = match list with
 [] -> infinity
 | (eki, kyori) :: rest -> if ( dest = eki ) then kyori else assoc dest rest ;;

(* 17.12 *)
(* insert1 ekikan_tree_t -> string -> string -> float -> ekikan_tree_t *)
let rec insert1 tree kiten shuten kyori = match tree with
  Empty -> Node( Empty, kiten, [(shuten,kyori)], Empty)
  | Node (left, n_kiten, n_list, right)
     -> if (kiten = n_kiten)
          then Node(left, n_kiten, (shuten,kyori)::n_list, right)
        else if (kiten < n_kiten)
          then Node(insert1 left kiten shuten kyori, n_kiten, n_list, right)
        else
          Node(left, n_kiten, n_list, insert1 right kiten shuten kyori);;

(* 双方向の駅間を木に挿入 *)
(* insert_ekikan ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let rec insert_ekikan tree ekikan = match ekikan with
  {kiten = ki; shuten = sh; kyori = ky } ->
    insert1 (insert1 tree sh ki ky) ki sh ky;;

(* 17.13 *)
(* inserts_ekikan ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan ekikan_tree ekikan_list
   = List.fold_right
      (fun ekikan tree -> insert_ekikan tree ekikan )
      ekikan_list ekikan_tree;;

(* 例外を使うよう変更 *)
(* get_ekikan_kyori string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori eki1 eki2 tree = match tree with
  Empty -> raise Not_found
  | Node (left, n_eki, n_list, right) ->
     if (eki1 = n_eki ) then assoc eki2 n_list
     else if (eki1 < n_eki) then get_ekikan_kyori eki1 eki2 left
     else get_ekikan_kyori eki1 eki2 right

let saitan_wo_bunri eki_list =
  List.fold_right (fun first (p, v) ->
		     match (first, p) with
		       ({namae = fn; saitan_kyori = fs; temae_list = ft},
			{namae = sn; saitan_kyori = ss; temae_list = st}) ->
			 if sn = "" then (first, v)
			 else if fs < ss then (first, p :: v)
			 else (p, first :: v))
		  eki_list
		  ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])

      let koushin p v ekikan_list = match p with
        {namae = pn; saitan_kyori = ps; temae_list = pt} ->
          List.map (fun q -> match q with
      	       {namae = qn; saitan_kyori = qs; temae_list = qt} ->
      		 let kyori = get_ekikan_kyori pn qn ekikan_list in
      		 if kyori = infinity
      		 then q
      		 else if ps +. kyori < qs
      		 then {namae = qn; saitan_kyori = ps +. kyori;
      				   temae_list = qn :: pt}
      		 else q)
      	     v

(* 例外を拾う *)
let koushin p v ekikan_tree = match p with
 {namae = pn; saitan_kyori = ps; temae_list = pt} ->
   List.map
    (fun q -> match q with
        {namae = qn; saitan_kyori = qs; temae_list = qt} ->
      try
        let kyori = get_ekikan_kyori pn qn ekikan_tree in
        if ps +. kyori < qs
        then {namae = qn; saitan_kyori = ps +. kyori;
              temae_list = qn :: pt}
        else q)
      with Not_found -> q)
    v

let rec dijkstra_main eki_list ekikan_tree = match eki_list with
   [] -> []
 | first :: rest ->
     let (saitan, nokori) = saitan_wo_bunri (first :: rest) in
     let eki_list2 = koushin saitan nokori ekikan_tree in
     saitan :: dijkstra_main eki_list2 ekikan_tree

let rec find shuten eki_list = match eki_list with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest ->
      if n = shuten then first else find shuten rest

(* 目的：始点と終点を受け取ったら、最短路を求め、終点のレコードを返す *)
(* dijkstra : string -> string -> eki_t *)
let dijkstra romaji_kiten romaji_shuten =
  let kiten = romaji_to_kanji romaji_kiten global_ekimei_list in
  let shuten = romaji_to_kanji romaji_shuten global_ekimei_list in
  let eki_list = make_initial_eki_list global_ekimei_list kiten in
  let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list in
  let eki_list2 = dijkstra_main eki_list global_ekikan_tree in
  find shuten eki_list2


(* テスト *)
let test1 = dijkstra "shibuya" "gokokuji" =
   {namae = "護国寺"; saitan_kyori = 9.8;
    temae_list =
      ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町";
       "青山一丁目"; "表参道"; "渋谷"]}
let test2 = dijkstra "myogadani" "meguro" =
   {namae = "目黒"; saitan_kyori = 12.7000000000000028;
    temae_list =
      ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王";
       "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]}
