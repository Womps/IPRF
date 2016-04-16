type word = char list;;

(* Cette fonction permet de savoir si le caractère donné en entrée est bien une lettre valide (lettre majuscule, minuscule, tiret et apostrophe)

   @param c Le caractère à analyser.
   @return booléen True si le caractère est une lettre valide, sinon false.
*)
let is_a_letter = fun c ->
  match c with
  | 'a'..'z' | 'A'..'Z' | '-' | '\''    -> true
  | _                                   -> false		 
;;

(* La fonction is_valid w permet de connaitre la validité d'un mot, par rapport à l'alphabet que nous avons constitué grâce à la fonction is_a_letter.

   @param w Le tableau contenant les caractères à évaluer.
   @return booléen True si le tableau est non vide et constitué de lettres valides, sinon false.
*)
let rec is_valid = fun w ->
  match w with
  | []                                  -> true
  | h::t                                -> if is_a_letter h then is_valid t else false
;;

let rec to_lower = fun w ->
  match w with
  | []                                  -> []
  | h::t                                -> char_of_int((int_of_char h)+ 32)::to_lower t
;;
