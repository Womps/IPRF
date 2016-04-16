type word = char list;;

let hello = (['H'; 'e'; 'l'; 'l'; 'o'] : word);;
let car1 = 'h';;
let car2 = '-';;
let car3 = '\'';;
let car4 = '4';;
let car5 = 'L';;

(* Cette fonction permet de savoir si le caractère donné en entrée est bien une lettre valide (lettre majuscule, minuscule, tiret et apostrophe)

   @param c Le caractère à analyser.
   @return booléen True si le caractère est une lettre valide, sinon false.
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

(* La fonction is_valid w permet de connaitre la validité d'un mot, par rapport à l'alphabet que nous avons constitué grâce à la fonction is_a_letter.

   @param w Le mot contenant les caractères à évaluer.
   @return booléen True si le mot est non vide et constitué de lettres valides, sinon false.
*)
let rec is_valid = fun (w : word) ->
  match w with
  | []                             -> false
  | [c]                            -> is_a_letter c
  | h::t                           -> if is_a_letter h then is_valid t else false
;;

(* Test de la fonction is_valid : *)
is_valid hello;;

(* Cette fonction permet de passer toutes les lettres d'un mot en minuscule. Celles qui étaient en majuscule, passeront en minuscule. Celles déjà en minuscule, le resteront.

   @param w Le mot à rendre en minuscule.
   @return word Le mot passé en minuscule.
*)
let rec to_lower = fun (w : word) ->
  match w with
  | []                                  -> ([] : word)
  | h::t                                -> (Char.lowercase h::to_lower t : word)
;;

(* Test de la fonction to_lower : *)
to_lower hello;;

(* La fonction trim supprime tous les caractères non valides, qui sont après la dernière lettre valide du mot w (en parcourant la liste de lettres de la gauche, vers la droite).

   @param w Le mot à traiter.
   @return word Retourne un mot.
*)
let rec trim = fun (w : word) ->
  match w with
  | []           -> ([] : word)
  | [h]          -> if is_a_letter h then ([h] : word) else ([] : word)
  | h::e::t      -> let queue = trim t in
		    if is_a_letter h && not(is_a_letter e) && is_valid queue then h::e::queue
		    else [h] 
;;

(* Test de la fonction trim : *)
let test1 = (['e'; 't'; 'c'; '.'; '.'; '.'] : word);;
let test2 = (['a'; '.'; 'b'] : word);;
let test3 = (['a'; '.'; 'b'; '.'; '.'; '.'] : word);;
trim test1;;
trim test2;;
trim test3;;
