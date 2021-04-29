(* global_ekikan_list, global_ekimei_list *)
#use "metro.ml"

let rec ekimei_insert lst ekimei0 = match lst with
    [] -> [ekimei0]
  | ({kanji = k; kana = a; romaji = r; shozoku = s} as ekimei) :: rest ->
      match ekimei0 with {kanji = k0; kana = a0; romaji = r0; shozoku = s0} ->
      	if a = a0 then ekimei_insert rest ekimei0
	      else if a < a0 then ekimei :: ekimei_insert rest ekimei0
	      else ekimei0 :: lst

let rec seiretsu ekimei_list = match ekimei_list with
    [] -> []
  | first :: rest -> ekimei_insert (seiretsu rest) first

let rec romaji_to_kanji r0 ekimei_list = match ekimei_list with
  [] -> ""
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest ->
    if r0 = r then k
    else romaji_to_kanji r0 rest

let rec get_ekikan_kyori eki1 eki2 lst = match lst with
  [] -> infinity
  | {kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j} :: rest ->
    if (eki1 = k && eki2 = s) || (eki1 = s && eki2 = k) then r
    else get_ekikan_kyori eki1 eki2 rest

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

let rec dijkstra_main eki_list ekikan_list = match eki_list with
   [] -> []
 | first :: rest ->
     let (saitan, nokori) = saitan_wo_bunri (first :: rest) in
     let eki_list2 = koushin saitan nokori ekikan_list in
     saitan :: dijkstra_main eki_list2 ekikan_list

let make_initial_eki_list ekimei_list kiten =
  List.map (fun ekimei -> match ekimei with
	     {kanji = k; kana = a; romaji = r; shozoku = s} ->
	       if k = kiten
	       then {namae = k; saitan_kyori = 0.; temae_list = [k]}
	       else {namae = k; saitan_kyori = infinity; temae_list = []})
	   ekimei_list

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
  let eki_list2 = dijkstra_main eki_list global_ekikan_list in
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
