(* Romain EPIARD

   Programmation Fonctionnelle (IPRF)
   Projet : Analyse d'un fichier texte en OCaml.

   === PARTIE 1 : ÉCHAUFFEMENT ===
   Type word, donné dans le sujet : 
*)
type word = char list;;

let hello = (['H'; 'e'; 'l'; 'l'; 'o'] : word);;
let car1 = 'h';;
let car2 = '-';;
let car3 = '\'';;
let car4 = '4';;
let car5 = 'L';;

(* Question 1 : Fonction is_a_letter

   Cette fonction permet de savoir si le caractère donné en entrée est bien une lettre valide (lettre majuscule, minuscule, tiret et apostrophe)

   is_a_letter : char -> bool = <fun>

   @param  c        Le caractère à analyser.
   @return booléen  True si le caractère est une lettre valide, sinon false.
*)
let is_a_letter = fun c ->
  match c with
  | 'a'..'z' | 'A'..'Z' | '-' | '\''    -> true
  | _                                   -> false		 
;;

(* Tests de la fonction is_a_letter : *)
is_a_letter car1;;
is_a_letter car2;;
is_a_letter car3;;
is_a_letter car4;;
is_a_letter car5;;

(* Question 2 : Fonction is_valid

   La fonction is_valid w permet de connaitre la validité d'un mot, par rapport à l'alphabet que nous avons constitué grâce à la fonction is_a_letter.

   is_valid : word -> bool = <fun>

   @param  w        Le mot contenant les caractères à évaluer.
   @return booléen  True si le mot est non vide et constitué de lettres valides, sinon false.
*)
let rec is_valid = fun (w : word) ->
  match w with
  | []                             -> false
  | [c]                            -> is_a_letter c
  | h::t                           -> if is_a_letter h then is_valid t else false
;;

(* Test de la fonction is_valid : *)
is_valid hello;;

(* Question 3 : Fonction to_lower

   Cette fonction permet de passer toutes les lettres d'un mot en minuscule. Celles qui étaient en majuscule, passeront en minuscule. Celles déjà en minuscule, le resteront.

   to_lower : word -> word = <fun>

   @param  w     Le mot à rendre en minuscule.
   @return word  Le mot passé en minuscule.
*)
let rec to_lower = fun (w : word) ->
  match w with
  | []                                  -> ([] : word)
  | h::t                                -> (Char.lowercase h::to_lower t : word)
;;

(* Test de la fonction to_lower : *)
to_lower hello;;

(* Question 4 : Fonction first_valid_char 

   La fonction first_valid_char parcourt un mot w. Elle s'arrête et retourne le mot w, dès qu'elle trouve une lettre valide.

   first_valid_char : word -> word = <fun>

   @param  w     Le mot à parcourir.
   @return word  Retourne un mot contenant la première lettre valide trouvée, ainsi que la suite du mot.
*)
let rec first_valid_char = fun w ->
  match w with
  | []       -> ([] : word)
  | [h]      -> if is_a_letter h then ([h] : word) else ([] : word)
  | h::t     -> if is_a_letter h then (w : word) else (first_valid_char t : word)
;;

(* Question 4 : Fonction trim

   La fonction trim utilise la fonction first_valid_char. Comme la fonction first_valid_char retourne la première lettre valide, avec la suite du mot, on lui passe notre mot à l'envers, afin de partir de la fin, pour s'arrêter dès que l'on croise une lettre valide, et ainsi supprimer toutes les lettres du mot, après la dernière lettre valide. Le retour de first_valid_char est de nouveau inversé, pour remettre le mot à l'endroit.

   trim : word -> word = <fun>

   @param  w     Le mot à traiter.
   @return word  Retourne le mot avec les caractères, suivant la dernière lettre valide, supprimés.
*)
let trim = fun (w : word) -> (List.rev (first_valid_char (List.rev w)) : word);;

(* Test de la fonction trim : *)
let test1 = (['e'; 't'; 'c'; '.'; '.'; '.'] : word);;
let test2 = (['a'; '.'; 'b'] : word);;
let test3 = (['a'; '.'; 'b'; '.'; '.'; '.'] : word);;
let test4 = (['H'; 'e'; 'l'; 'l'; 'o'; ','; ' '; 'n'; 'i'; 'c'; 'e'; ' '; 't'; 'o'; ' '; 'm'; 'e'; 'e'; 't'; ' '; 'y'; 'o'; 'u'; '.'; '.'; '!'] : word);;
trim test1;;
trim test2;;
trim test3;;
trim test4;;

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


(* get_words: string -> word list *)
let get_words = fun file ->
  let lc  = handle_file (update_acc []) file in
  let lw  = words lc [] [] in
  let lw' = List.map (fun w -> to_lower (trim w)) lw in
  List.filter is_valid lw'
;;

(* === PARTIE 2 : RÉCUPÉRATION DE LA LISTE DES MOTS D'UN FICHIER TEXTE ===

   Question 5 : Fonction print_word

   Cette fonction est utilisée pour afficher le mot contenu dans la liste en entrée. 

   print_word : word -> unit = <fun>

   @param  w     Le mot à afficher.
   @return unit  Ne retourne rien.
*)
let print_word = fun (w : word) -> List.iter (Printf.printf "%c") w;;

(* Question 5 : Fonction print_text

   Cette fonction permet d'afficher chaque mot contenu dans une liste de mots. Cela nous permet ainsi de tester le code fourni et la fonction print_word.

   print_text : word list -> unit = <fun>
   
   @param  l     Liste de liste. On a une liste de mots en entrée.
   @return unit  Ne retourne rien.
*)
let print_text = fun l -> List.iter print_word l;;

(* Test de la fonction print_word : *)
print_word hello;;

(* Test de la fonction print_text : 
   On commence par récupérer la liste de mots contenue dans le fichier nommé fichier.txt.
*)
let text = get_words "fichier.txt";;

(* On appelle la fonction print_text, pour afficher chaque mot de la liste. *)
print_text text;;

(* Question 7 : Fonction nub

   La fonction nub prend une liste en entrée, et retourne une liste, avec les doublons en moins. Pour ce faire, on utilise la fonction List.mem, pour vérifier si l'élément sur lequel on est, existe dans la liste. On aurait aussi pu implémenter une fonction de recherche. Mais afin de gagner du temps, on utilise la fonction prévue dans le module List.

   nub : 'a list -> 'a list = <fun>

   @param   l       La liste, dans laquelle il faut retirer les doublons.
   @return 'a list  Retourne la liste donnée en entrée, avec les doublons en moins.
*)
let rec nub = fun l ->
  match l with
  | []     -> []
  | h::t   -> if List.mem h t then nub t else h::nub t
;;

(* Test de la fonction nub : *)
nub hello;;

(* Question 8 : Fonction count_words

   La fonction count_words récupère une liste de mots dans un fichier, sur la donnée d'un chemin vers un fichier texte. On aurait pu utiliser des List.fold_left, ou faire des fonctions auxiliaires pour compter le nombre de mots valides et de mots valides différents. Comme la liste de mots issue de get_words est une liste de mots valides uniquement (il n'y a pas de mots invalides, en sortie de cette fonction), nous pouvons directement utiliser List.length, sans avoir besoin de reparcourir la liste, et de vérifier tous les mots uns par uns.

   Retourne un couple (m, n) où :
           * m est le nombre de mots valides dans le fichier,
           * n est le nombre de mots valides différents dans le fichier.

   count_words : string -> int * int = <fun>

   @param   file   Le chemin vers un fichier contenant du texte.
   @return  tuple  Retourne un couple d'entiers (m, n).
*)
let count_words = fun file ->
  let text = get_words file in
  let m = List.length text in
  let n = List.length (nub text) in
  (m, n)
;;

count_words "fichier.txt";;
count_words "test.txt";;
count_words "lorem.txt";;

(* === PARTIE 3 : LA STRUCTURE DE TRIE === *)
module CharMap = Map.Make (Char) ;;
type trie = T of int * trie CharMap.t ;;

(* Déclaration d'un Trie vide : *)
let empty_trie = T (0, CharMap.empty) ;;

(* Construction du Trie de la figure 1, de façon non efficace : *)
let ns = T (1, CharMap.empty);;
let ne = T (2, CharMap.add 's' ns CharMap.empty);;
let na = T (1, CharMap.empty);;
let nl = T (0, CharMap.add 'a' na (CharMap.add 'e' ne CharMap.empty));; 
let nn = T (1, CharMap.empty);;
let nu = T (0, CharMap.add 'n' nn CharMap.empty);;
let example = T (0, CharMap.add 'l' nl (CharMap.add 'u' nu CharMap.empty));;

(* Question 10 : Fonction trie_get 
   
   La fonction trie_get prend un mot w et un trie t en entrée, pour renvoyer la valeur associée à w, dans t. Si le mot w ne peut pas être retrouvé dans l'arbre (aucun fils ne correspond à la lettre que l'on cherche à lire), alors on renvoie la valeur 0.

   trie_get : word -> trie -> int = <fun>

   @param  w    Le mot à chercher dans l'arbre.
   @param  t    L'arbre t dans lequel chercher le mot w.
   @return int  La valeur associée au mot w, dans l'arbre t.
*)
let rec trie_get = fun (w : word) -> fun t ->
  match t with
  | T (x, y)      -> if (List.length w) < 1 then x
                     else
                         if (CharMap.mem (List.hd w) y) then
	                   trie_get (List.tl w) (CharMap.find (List.hd w) y)
                         else
                             0
;;

(* Test de la fonction trie_get : *)
trie_get ['l'; 'e'; 's'] example;;
trie_get ['l'; 'e'] example;;

(* Question 11 : Fonction trie_incr 

   La fonction trie_incr prend un mot w et un trie tr en entrée, pour renvoyer un nouveau trie tr' dans lequel la valeur associée à w a été augmentée de 1.

   trie_incr : word -> trie -> trie = <fun>

   @param  w    Le mot pour lequel on veut incrémenter de 1 la valeur dans l'arbre.
   @param  t    L'arbre dans lequel on veut incrémenter la valeur de w.
   @return trie L'arbre avec la nouvelle valeur associée à w.
*)
let rec trie_incr = fun (w : word) -> fun tr ->
  match tr with
  | T (v, m)        -> match w with
		               | []           -> T (v+1, m)
                       | h::t         -> let s = if CharMap.mem h m then CharMap.find h m else T (0, CharMap.empty) in
										 let s' = trie_incr t s in
										 let m' = CharMap.add h s' m in
										 T(v, m')
;;

(* Test de la fonction trie_incr : *)
trie_get ['l'; 'e'; 's'] example;;
let newTrie = trie_incr ['l'; 'e'; 's'] example;;
trie_get ['l'; 'e'; 's'] newTrie;;

let rec trie_construction = fun text -> fun tr ->
	match text with
	| []       -> T (0, CharMap.empty)
	| h::t     -> let tr' = trie_incr h tr in
				  let final_trie = trie_construction t tr' in
				  final_trie
;;


(* Question 12 : Fonction trie_words

   Fonction qui prend en entrée un chemin vers un fichier, pour renvoyer le trie construit à partir des mots de ce fichier.
   
   trie_words : string -> trie = <fun>
   
   @param  s    La chaîne de caractères correspondant au chemin du fichier à lire.
   @return trie L'arbre construite à partir des mots du fichier donné en entrée.
*)
let trie_words = fun s -> fun tr ->
  let text = get_words s in
  let empty_trie = T (0, CharMap.empty) in
  let add = trie_construction text empty_trie in
  add
;;

(* Test de la fonction trie_words : *)
let trie = trie_words "fichier.txt";;
trie_get ['c'; 'o'; 'd'; 'e'] trie;;

let trie_card_test = fun key -> fun data -> fun a ->
	
;;

(* Question 13 : Fonction trie_card
*)
let rec trie_card = fun trie ->
  match trie with
  | T (v, m)        -> if CharMap.is_empty m then 0 else 1 + CharMap.fold trie_card m 0
;;
