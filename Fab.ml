
type word = char list;;
(*Question 1 *)
let is_a_letter x = x='\'' || x='-' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z');;
	
(*
is_a_letter '_' ;; 
is_a_letter 'd' ;; 
is_a_letter 'c' ;; 
is_a_letter '\'' ;; 
*)


(*Question 2 *)
let rec is_valid = fun (w : word) -> match w with
	[] -> false
	| h::[] -> is_a_letter h 
	| h::t -> if is_a_letter h 
	            then is_valid t
			else false;;

(*			
let tete = is_valid [];; 
Printf.printf "%b\n" tete;;
*)

(*Question 3 *)
let rec to_lower = fun (w : word) -> match w with
	[] -> []
	| h::t -> (Char.lowercase h :: to_lower t : word);;


(*
let rec printttt = fun w -> match w with
			[] -> []
			| h::t -> Printf.printf "%c" h; printttt t ;;

let word = ['a';'A';'b'];;
printttt word;;
let word = to_lower word;;
printttt word;;
*)


(*Question 4 *)
(*
let rec trim = fun (w : word) -> match List.rev w with 
			 [] -> []
			| h::t -> if is_a_letter h 
			            then (w : word)
					else trim (List.rev t) ;;
*)
					
let rec trim = fun (w : word) ->
    let l = List.rev w in
    match l with
        [] -> []
        | h::t -> if is_a_letter h
                    then (w : word)
                else
                    trim (List.rev t)
    ;;
	
(*)	
let resut = trim ['V';'A';'b' ; '.';'.'; 'a'; '.'; '(';'\n';' ';'a';'f';' ';'f';'d';'\n'];;
let resut = trim ['.';'V';'A';'b' ; '.';'.'; 'a'; '.';' ';'\n';' '];;
printttt resut;;
*)






(* handle_file: (in_channel -> 'a) -> string -> 'a *)
let handle_file = fun f -> fun file ->
  let input = open_in file in
  let res = f input in
  let _ = close_in input in
  res
;;

(* my_input_char: in_channel -> char option *)
let get_char = fun input ->
  try  Some (input_char input)
  with End_of_file -> None
;;

(* update_acc: word -> in_channel -> word *)
let rec update_acc = fun acc -> fun input ->
  match get_char input with
  | None -> acc
  | Some c -> update_acc (c::acc) input
 ;;

(* words: char list -> word -> word list -> word list *)
let rec words = fun l -> fun w -> fun acc ->
  match l, w with
  | [], []    -> acc
  | [], _     -> w :: acc
  | c::cs, [] -> if c=' ' then words cs [] acc
                 else words cs [c] acc
  | c::cs, _  -> if c=' ' then words cs [] (w::acc)
                 else words cs (c::w) acc
;;

(* words: char list -> word -> word list -> word list 
let rec words = fun l -> fun w -> fun acc ->
  match l, w with
  | [], []    -> acc
  | [], _     -> w :: acc
  | c::cs, [] -> if c=' ' || c='\n' then words cs [] acc
                 else words cs [c] acc
  | c::cs, _  -> if c=' ' || c='\n' then words cs [] (w::acc)
                 else words cs (c::w) acc
;;*)


(* get_words: string -> word list *)
let get_words = fun file ->
  let lc  = handle_file (update_acc []) file in
  let lw  = words lc [] [] in
  let lw' = List.map (fun w -> to_lower (trim w)) lw in
  List.filter is_valid lw'
;;






(*Question 5 *)
(*
let rec print_word = fun (w : word) -> match w with
				[] -> []
				| h::t -> Printf.printf "%c" h; print_word t ;;
				
let rec print_word2 = fun w -> match w with
				[] -> []
				| h::t -> print_word h; Printf.printf " "; print_word2 t ;;
*)
let rec print_word = fun (w : word) -> List.iter (Printf.printf "%c") w;;
				
let rec print_word2 = fun lw -> List.iter print_word lw;;

(*				
let teteee = get_words "H:\\IPRF\\test.txt";;	
print_word2 teteee;;
*)

(*Question 6 .... *)


(*Question 7 *)
let rec nub = fun l ->
  match l with
  | []     -> []
  | h::t   -> if List.mem h t
				then nub t
			else h::nub t
;;

(*Question 8 *)
let rec count_w = fun lw ->
  match lw with
	[] -> 0
	| h::t -> 1+(count_w t)
;;

let rec count_words = fun s ->
  let wordst = get_words s in
  (count_w wordst, count_w (nub wordst))
;;

(*
let count_words = fun s ->
  let lw = get_words s in
  let a = List.length lw in
  let b = List.length (lw) in
  (a, b)
;;
*)

(*Question 9 .... *)


(*Question 10 *)
module CharMap = Map.Make (Char);;
type trie = T of int * trie CharMap.t;;
let empty_trie = T (0, CharMap.empty);;


(* alti => itla *)
let alti1 = T (1, CharMap.empty);; (* h::t *)
let alti2 = T (0, CharMap.add 'i' alti1 CharMap.empty);; (* h::t *)
let alti3 = T (2, CharMap.add 'v' alti2 CharMap.empty);; (* h::t *)
let alti33 = T (2, CharMap.add 'v' alti3 CharMap.empty);; (* h::t *)

let alti16 = T (4, CharMap.empty);; (* h::t *)
let alti26 = T (0, CharMap.add 'i' alti16 CharMap.empty);; (* h::t *)
let alti36 = T (2, CharMap.add 'v' alti26 CharMap.empty);; (* h::t *)

(* alba => abla *)
let alba1 = T (1, CharMap.empty);;
let alba2 = T (0, CharMap.add 'a' alba1 CharMap.empty);; (* h::t *)
let alba22 = T (3, CharMap.add 'h' alba2 CharMap.empty);; (* h::t *)
(*let alba3 = T (0, CharMap.add 'b' alba2 CharMap.empty);;  h::t *)
let alba3 = T (0, CharMap.add 't' alti33 (CharMap.add 'b' alba22 (CharMap.add 'v' alti36 CharMap.empty)));; (* h::[] *)
let alba4 = T (1, CharMap.add 'l' alba3 CharMap.empty);; (* h::t *)
let racine = T (0, CharMap.add 'a' alba4 CharMap.empty);; (* h::t *)


let rec trie_get = fun (w : word) (tr : trie) ->
    match tr with
        T (x, y) -> match w with
                    [] -> x
                    | h::t -> if CharMap.mem h y 
                                then trie_get t (CharMap.find h y )
                            else 
                                0
;;



(*Question 11 *)
let rec trie_incr = fun (w : word) (t : trie) ->
    match t with 
		T (v, m) ->	match w with
						[] -> T ((1+v), m)
						| x::xs -> let s = (if CharMap.mem x m then CharMap.find x m else T (0, CharMap.empty)) in
									let s' = trie_incr (xs : word) s in
									let m' = CharMap.add x s' m in
									T (v, m')
;;

(*
trie_get ['a'; 'l'; 'b'] racine;;
trie_get ['a'; 'l'; 'k'] racine;;
let racine = trie_incr ['a'; 'l'; 'b'] racine;;
trie_get ['a'; 'l'; 'k'] racine;;
trie_get ['a'; 'l'; 'b'] racine;;
trie_get ['a'; 'l'] racine;;
trie_get ['a'] racine;;
*)

(*Question 12 *)
let rec add_words = fun lw -> fun (tr : trie) ->
  match lw with
	[] -> tr
	| h::t -> add_words t (trie_incr h tr); 
;;

let trie_words = fun s ->
  let wordst = get_words s in
  let trie = T (0, CharMap.empty) in
  add_words wordst trie
;;

(*
print_word2 (get_words "H:\\IPRF\\test.txt");;
let trie = trie_words "H:\\IPRF\\test.txt";;
trie_get ['v'; 't'; 't'] trie;;
trie_get ['v'; 't'; 'e'] trie;;
trie_get ['t'; 'e'; 'x'; 't'] trie;;
*)

(*Question 13 *)
(*
let rec trie_card' = fun (tr : trie) ->
  match tr with
	T (v, m) -> List.fold_right (
								fun (a, b) c -> c+1+trie_card' b
								) (CharMap.bindings m) 0
;;
*)
let rec trie_card = fun (tr : trie) ->
  match tr with
	T (v, m) -> List.fold_right (
								fun (a, b) c -> (match b with T(x, y) -> if x = 0 then 0 else 1)+c+trie_card b
								) (CharMap.bindings m) 0
;;

(*Question 14 *)
let rec trie_sum = fun (tr : trie) ->
  match tr with
	T (v, m) -> List.fold_right (
								fun (a, b) c -> (match b with T(x, y) -> x)+c+trie_sum b
								) (CharMap.bindings m) 0
;;
(*
trie_card racine;;
trie_card' racine;;
trie_sum racine;;
*)

(*Question 15 *)

type word = char list;;
(*Question 1 *)
let is_a_letter x = x='\'' || x='-' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z');;
	
(*
is_a_letter '_' ;; 
is_a_letter 'd' ;; 
is_a_letter 'c' ;; 
is_a_letter '\'' ;; 
*)


(*Question 2 *)
let rec is_valid = fun (w : word) -> match w with
	[] -> false
	| h::[] -> is_a_letter h 
	| h::t -> if is_a_letter h 
	            then is_valid t
			else false;;

(*			
let tete = is_valid [];; 
Printf.printf "%b\n" tete;;
*)

(*Question 3 *)
let rec to_lower = fun (w : word) -> match w with
	[] -> []
	| h::t -> (Char.lowercase h :: to_lower t : word);;


(*
let rec printttt = fun w -> match w with
			[] -> []
			| h::t -> Printf.printf "%c" h; printttt t ;;

let word = ['a';'A';'b'];;
printttt word;;
let word = to_lower word;;
printttt word;;
*)


(*Question 4 *)
(*
let rec trim = fun (w : word) -> match List.rev w with 
			 [] -> []
			| h::t -> if is_a_letter h 
			            then (w : word)
					else trim (List.rev t) ;;
*)
					
let rec trim = fun (w : word) ->
    let l = List.rev w in
    match l with
        [] -> []
        | h::t -> if is_a_letter h
                    then (w : word)
                else
                    trim (List.rev t)
    ;;
	
(*)	
let resut = trim ['V';'A';'b' ; '.';'.'; 'a'; '.'; '(';'\n';' ';'a';'f';' ';'f';'d';'\n'];;
let resut = trim ['.';'V';'A';'b' ; '.';'.'; 'a'; '.';' ';'\n';' '];;
printttt resut;;
*)






(* handle_file: (in_channel -> 'a) -> string -> 'a *)
let handle_file = fun f -> fun file ->
  let input = open_in file in
  let res = f input in
  let _ = close_in input in
  res
;;

(* my_input_char: in_channel -> char option *)
let get_char = fun input ->
  try  Some (input_char input)
  with End_of_file -> None
;;

(* update_acc: word -> in_channel -> word *)
let rec update_acc = fun acc -> fun input ->
  match get_char input with
  | None -> acc
  | Some c -> update_acc (c::acc) input
 ;;

(* words: char list -> word -> word list -> word list *)
let rec words = fun l -> fun w -> fun acc ->
  match l, w with
  | [], []    -> acc
  | [], _     -> w :: acc
  | c::cs, [] -> if c=' ' then words cs [] acc
                 else words cs [c] acc
  | c::cs, _  -> if c=' ' then words cs [] (w::acc)
                 else words cs (c::w) acc
;;

(* words: char list -> word -> word list -> word list 
let rec words = fun l -> fun w -> fun acc ->
  match l, w with
  | [], []    -> acc
  | [], _     -> w :: acc
  | c::cs, [] -> if c=' ' || c='\n' then words cs [] acc
                 else words cs [c] acc
  | c::cs, _  -> if c=' ' || c='\n' then words cs [] (w::acc)
                 else words cs (c::w) acc
;;*)


(* get_words: string -> word list *)
let get_words = fun file ->
  let lc  = handle_file (update_acc []) file in
  let lw  = words lc [] [] in
  let lw' = List.map (fun w -> to_lower (trim w)) lw in
  List.filter is_valid lw'
;;






(*Question 5 *)
(*
let rec print_word = fun (w : word) -> match w with
				[] -> []
				| h::t -> Printf.printf "%c" h; print_word t ;;
				
let rec print_word2 = fun w -> match w with
				[] -> []
				| h::t -> print_word h; Printf.printf " "; print_word2 t ;;
*)
let rec print_word = fun (w : word) -> List.iter (Printf.printf "%c") w;;
				
let rec print_word2 = fun lw -> List.iter print_word lw;;

(*				
let teteee = get_words "H:\\IPRF\\test.txt";;	
print_word2 teteee;;
*)

(*Question 6 .... *)


(*Question 7 *)
let rec nub = fun l ->
  match l with
  | []     -> []
  | h::t   -> if List.mem h t
				then nub t
			else h::nub t
;;

(*Question 8 *)
let rec count_w = fun lw ->
  match lw with
	[] -> 0
	| h::t -> 1+(count_w t)
;;

let rec count_words = fun s ->
  let wordst = get_words s in
  (count_w wordst, count_w (nub wordst))
;;

(*
let count_words = fun s ->
  let lw = get_words s in
  let a = List.length lw in
  let b = List.length (lw) in
  (a, b)
;;
*)

(*Question 9 .... *)


(*Question 10 *)
module CharMap = Map.Make (Char);;
type trie = T of int * trie CharMap.t;;
let empty_trie = T (0, CharMap.empty);;


(* alti => itla *)
let alti1 = T (30, CharMap.empty);; (* h::t *)
let alti2 = T (20, CharMap.add 'i' alti1 CharMap.empty);; (* h::t *)
let alti3 = T (21, CharMap.add 'v' alti2 CharMap.empty);; (* h::t *)
let alti33 = T (2, CharMap.add 'v' alti3 CharMap.empty);; (* h::t *)

let alti16 = T (4, CharMap.empty);; (* h::t *)
let alti26 = T (0, CharMap.add 'i' alti16 CharMap.empty);; (* h::t *)
let alti36 = T (2, CharMap.add 'v' alti26 CharMap.empty);; (* h::t *)

(* alba => abla *)
let alba1 = T (1, CharMap.empty);;

let alba22 = T (3, CharMap.add 'h' alba1 CharMap.empty);; (* h::t *)
(*let alba3 = T (0, CharMap.add 'b' alba2 CharMap.empty);;  h::t *)
let alba3 = T (0, CharMap.add 't' alti33 (CharMap.add 'b' alba22 (CharMap.add 'v' alti36 CharMap.empty)));; (* h::[] *)
let alba4 = T (1, CharMap.add 'l' alba3 CharMap.empty);; (* h::t *)
let racine = T (0, CharMap.add 'a' alba4 CharMap.empty);; (* h::t *)


let rec trie_get = fun (w : word) (tr : trie) ->
    match tr with
        T (x, y) -> match w with
                    [] -> x
                    | h::t -> if CharMap.mem h y 
                                then trie_get t (CharMap.find h y )
                            else 
                                0
;;



(*Question 11 *)
let rec trie_incr = fun (w : word) (t : trie) ->
    match t with 
		T (v, m) ->	match w with
						[] -> T ((1+v), m)
						| x::xs -> let s = (if CharMap.mem x m then CharMap.find x m else T (0, CharMap.empty)) in
									let s' = trie_incr (xs : word) s in
									let m' = CharMap.add x s' m in
									T (v, m')
;;

(*
trie_get ['a'; 'l'; 'b'] racine;;
trie_get ['a'; 'l'; 'k'] racine;;
let racine = trie_incr ['a'; 'l'; 'b'] racine;;
trie_get ['a'; 'l'; 'k'] racine;;
trie_get ['a'; 'l'; 'b'] racine;;
trie_get ['a'; 'l'] racine;;
trie_get ['a'] racine;;
*)

(*Question 12 *)
let rec add_words = fun lw -> fun (tr : trie) ->
  match lw with
	[] -> tr
	| h::t -> add_words t (trie_incr h tr); 
;;

let trie_words = fun s ->
  let wordst = get_words s in
  let trie = T (0, CharMap.empty) in
  add_words wordst trie
;;

(*
print_word2 (get_words "H:\\IPRF\\test.txt");;
let trie = trie_words "H:\\IPRF\\test.txt";;
trie_get ['v'; 't'; 't'] trie;;
trie_get ['v'; 't'; 'e'] trie;;
trie_get ['t'; 'e'; 'x'; 't'] trie;;
*)

(*Question 13 *)
(*
let rec trie_card' = fun (tr : trie) ->
  match tr with
	T (v, m) -> List.fold_right (
								fun (a, b) c -> c+1+trie_card' b
								) (CharMap.bindings m) 0
;;
*)
let rec trie_card = fun (tr : trie) ->
  match tr with
	T (v, m) -> List.fold_right (
								fun (a, b) c -> (match b with T(x, y) -> if x = 0 then 0 else 1)+c+trie_card b
								) (CharMap.bindings m) 0
;;

(*Question 14 *)
let rec trie_sum = fun (tr : trie) ->
  match tr with
	T (v, m) -> List.fold_right (
								fun (a, b) c -> (match b with T(x, y) -> x)+c+trie_sum b
								) (CharMap.bindings m) 0
;;
(*
trie_card racine;;
trie_card' racine;;
trie_sum racine;;
*)

(*Question 15 *)
let rec count_words_v2 = fun s ->
  let wordst = trie_words s in
	let n = trie_card wordst in
	let m = trie_sum wordst in
  (m, n)
  ;;
  
(*
  count_words_v2 "G:\\IPRF\\test.txt";;
*)

(*Question 16 *)
(* Mot le plus long et sa taille*)
let rec trie_word_most = fun ta (w : word) (tr : trie) ->
	match tr with
	T (v, m) -> if CharMap.is_empty m 
					then (ta, w)
				else
					List.fold_right (
						fun (a, b) (tai, wor) -> match trie_word_most (ta+1) (a::w) b with
													(u, v) -> if u > tai then (u, v) else (tai, wor) 
					) (CharMap.bindings m) (0,[])
;;
(* trie_word_most 0 [] racine;; *)


(* Mot le plus utilisé et son nombre d'occurance*)
let rec trie_use_occu = fun (w : word) (tr : trie) ->
	match tr with
	T (v, m) -> if CharMap.is_empty m 
					then (v, w)
				else
					List.fold_right (
						fun (a, b) (oc, wo) -> match trie_use_occu (a::w) b with
												(p, q) -> if v > oc || p > oc
															then if v > p
																	then (p, q)
																else 
																	(v, w)
														else
															(oc, wo)
					) (CharMap.bindings m) (0,[])
;;
(* trie_use_occu [] racine;; *)

