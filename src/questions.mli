(* Romain EPIARD

   Programmation Fonctionnelle (IPRF)
   Projet : Analyse d'un fichier texte en OCaml.

   Interface du module questions.
*)
type word

(* Définition de différentes variables, qui nous permettront de réaliser divers tests : *)
val hello : word
val car1 : char
val car2 : char
val car3 : char
val car4 : char
val car5 : char

(* Question 1 : Fonction is_a_letter

   Cette fonction permet de savoir si le caractère donné en entrée est bien une lettre valide (lettre majuscule, minuscule, tiret et apostrophe)

   is_a_letter : char -> bool = <fun>

   @param  c        Le caractère à analyser.
   @return booléen  True si le caractère est une lettre valide, sinon false.
*)
val is_a_letter : char -> bool

(* Question 2 : Fonction is_valid

   La fonction is_valid w permet de connaitre la validité d'un mot, par rapport à l'alphabet que nous avons constitué grâce à la fonction is_a_letter.

   is_valid : word -> bool = <fun>

   @param  w        Le mot contenant les caractères à évaluer.
   @return booléen  True si le mot est non vide et constitué de lettres valides, sinon false.
*)
val is_valid : word -> bool

(* Question 3 : Fonction to_lower

   Cette fonction permet de passer toutes les lettres d'un mot en minuscule. Celles qui étaient en majuscule, passeront en minuscule. Celles déjà en minuscule, le resteront.

   to_lower : word -> word = <fun>

   @param  w     Le mot à rendre en minuscule.
   @return word  Le mot passé en minuscule.
*)
val to_lower : word -> word

(* Question 4 : Fonction first_valid_char (Partie 1/2)

   La fonction first_valid_char parcourt un mot w. Elle s'arrête et retourne le mot w, dès qu'elle trouve une lettre valide.

   first_valid_char : word -> word = <fun>

   @param  w     Le mot à parcourir.
   @return word  Retourne un mot contenant la première lettre valide trouvée, ainsi que la suite du mot.
*)
val first_valid_char : word -> word

(* Question 4 : Fonction trim (Partie 2/2)

   La fonction trim utilise la fonction first_valid_char. Comme la fonction first_valid_char retourne la première lettre valide, avec la suite du mot, on lui passe notre mot à l'envers, afin de partir de la fin, pour s'arrêter dès que l'on croise une lettre valide, et ainsi supprimer toutes les lettres du mot, après la dernière lettre valide. Le retour de first_valid_char est de nouveau inversé, pour remettre le mot à l'endroit.

   trim : word -> word = <fun>

   @param  w     Le mot à traiter.
   @return word  Retourne le mot avec les caractères, suivant la dernière lettre valide, supprimés.
*)
val trim : word -> word

(* Définition de quelques variables, qui nous permettront de réaliser divers tests : *)
val test1 : word
val test2 : word
val test3 : word
val test4 : word

(* handle_file : (in_channel -> 'a) -> string -> 'a *)
val handle_file : (in_channel -> 'a) -> string -> 'a

(* my_input_char: in_channel -> char option *)
val get_char : in_channel -> char option

(* update_acc: word -> in_channel -> word *)
val update_acc : word -> in_channel -> word 

(* words: char list -> word -> word list -> word list *)
val words : char list -> word -> word list -> word list

(* get_words: string -> word list *)
val get_words : string -> word list

(* === PARTIE 2 : RÉCUPÉRATION DE LA LISTE DES MOTS D'UN FICHIER TEXTE ===

   Question 5 : Fonction print_word (Partie 1/2)

   Cette fonction est utilisée pour afficher le mot contenu dans la liste en entrée. 

   print_word : word -> unit = <fun>

   @param  w     Le mot à afficher.
   @return unit  Ne retourne rien.
*)
val print_word : word -> unit

(* Question 5 : Fonction print_text (Partie 2/2)

   Cette fonction permet d'afficher chaque mot contenu dans une liste de mots. Cela nous permet ainsi de tester le code fourni et la fonction print_word.

   print_text : word list -> unit = <fun>
   
   @param  l     Liste de liste. On a une liste de mots en entrée.
   @return unit  Ne retourne rien.
*)
val print_text : word list -> unit

(* Test de la fonction print_text : 
   On commence par récupérer la liste de mots contenue dans le fichier nommé fichier.txt.
*)
val text : word list

(* Question 7 : Fonction nub

   La fonction nub prend une liste en entrée, et retourne une liste, avec les doublons en moins. Pour ce faire, on utilise la fonction List.mem, pour vérifier si l'élément sur lequel on est, existe dans la liste. On aurait aussi pu implémenter une fonction de recherche. Mais afin de gagner du temps, on utilise la fonction prévue dans le module List.

   nub : 'a list -> 'a list = <fun>

   @param   l       La liste, dans laquelle il faut retirer les doublons.
   @return 'a list  Retourne la liste donnée en entrée, avec les doublons en moins.
*)
val nub : 'a list -> 'a list

(* Question 8 : Fonction count_words

   La fonction count_words récupère une liste de mots dans un fichier, sur la donnée d'un chemin vers un fichier texte. On aurait pu utiliser des List.fold_left, ou faire des fonctions auxiliaires pour compter le nombre de mots valides et de mots valides différents. Comme la liste de mots issue de get_words est une liste de mots valides uniquement (il n'y a pas de mots invalides, en sortie de cette fonction), nous pouvons directement utiliser List.length, sans avoir besoin de reparcourir la liste, et de vérifier tous les mots uns par uns.

   Retourne un couple (m, n) où :
           * m est le nombre de mots valides dans le fichier,
           * n est le nombre de mots valides différents dans le fichier.

   count_words : string -> (int * int) = <fun>

   @param   file   Le chemin vers un fichier contenant du texte.
   @return  tuple  Retourne un couple d'entiers (m, n).
*)
val count_words : string -> (int * int)

(* === PARTIE 3 : LA STRUCTURE DE TRIE === *)
type trie

(* Déclaration d'un Trie vide : *)
val empty_trie : trie

(* Construction du Trie de la figure 1, de façon non efficace : *)
val ns : trie
val ne : trie
val na : trie
val nl : trie
val nn : trie
val nu : trie
val example : trie

(* Question 10 : Fonction trie_get 
   
   La fonction trie_get prend un mot w et un trie t en entrée, pour renvoyer la valeur associée à w, dans t. Si le mot w ne peut pas être retrouvé dans l'arbre (aucun fils ne correspond à la lettre que l'on cherche à lire), alors on renvoie la valeur 0.

   trie_get : word -> trie -> int = <fun>

   @param  w    Le mot à chercher dans l'arbre.
   @param  t    L'arbre t dans lequel chercher le mot w.
   @return int  La valeur associée au mot w, dans l'arbre t.
*)
val trie_get : word -> trie -> int

(* Question 11 : Fonction trie_incr 

   La fonction trie_incr prend un mot w et un trie tr en entrée, pour renvoyer un nouveau trie tr' dans lequel la valeur associée à w a été augmentée de 1.

   trie_incr : word -> trie -> trie = <fun>

   @param  w    Le mot pour lequel on veut incrémenter de 1 la valeur dans l'arbre.
   @param  t    L'arbre dans lequel on veut incrémenter la valeur de w.
   @return trie L'arbre avec la nouvelle valeur associée à w.
*)
val trie_incr : word -> trie -> trie

(* Test de la fonction trie_incr : *)
val newTrie : trie

(* Question 12 : Fonction trie_construction

   Cette fonction va nous permettre de parcourir la liste de mots text en entrée, et d'ajouter chaque mots à l'arbre tr.
   
   trie_construction : word list -> trie = <fun>
   
   @param  text La liste de mots à ajouter à l'arbre tr.
   @return trie L'arbre construit à partir de la liste de mots.
*)
val trie_build : trie -> word -> in_channel -> trie

(* Question 12 : Fonction trie_words

   Fonction qui prend en entrée un chemin vers un fichier, pour renvoyer le trie construit à partir des mots de ce fichier.
   
   trie_words : string -> trie = <fun>
   
   @param  s    La chaîne de caractères correspondant au chemin du fichier à lire.
   @return trie L'arbre construite à partir des mots du fichier donné en entrée.
*)
val trie_words : string -> trie

(* Test de la fonction trie_words : *)
val test_trie_words : trie

(* Question 13 : Fonction trie_card

   Cette fonction compte le nombre de noeuds qui ont une valeur non nulle dans un trie donné.

   trie_card : trie -> int = <fun>
   
   @param   tr   L'arbre pour lequel on doit compter les noeuds avec une valeur non nulle.
   @return  int  Le nombre entier, correspondant au cardinal des noeuds avec une valeur non nulle dans l'arbre tr.
*)
val trie_card : trie -> int

(* Question 14 : Fonction trie_sum

   Cette fonction retourne la somme des valeurs contenues dans les noeuds d'un trie donné.

   trie_sum : trie -> int = <fun>
   
   @param   tr   L'arbre pour lequel on doit faire la somme des valeurs contenues dans le trie.
   @return  int  Le nombre entier, correspondant à la somme des valeurs contenues dans le trie.
*)
val trie_sum : trie -> int

(* Question 15 : Fonction count_words_v2 
   
   La fonction count_words_v2 retourne la même chose que count_words mais en utilisant les trie que nous venons d'implémenter, au lieu de listes.

   Retourne un couple (m, n) où :
	   * m est le nombre de mots valides dans le fichier,
	   * n est le nombre de mots valides différents dans le fichier.
   
   count_words_v2 : string -> (int * int) = <fun>
   
   @param   f     Le fichier à lire en entrée.
   @return  tuple Retourne un couple d'entiers (m, n).
*)
val count_words_v2 : string -> (int * int)

(* Question 16 : Fonction trie_longer_word (Question N°3 de l'introduction)

   Cette fonction retourne un couple, le premier élément étant le mot le plus long dans l'arbre. Le deuxième étant sa taille.
   
   trie_longer_word : trie -> (word * int) = <fun>

   @param   tr    Le trie dans lequel chercher le mot le plus long.
   @return  tuple Retourne un couple (word * int), word étant le mot trouvé, et l'entier, sa taille.
*)
val trie_longer_word : trie -> (word * int)

(* Test de la fonction trie_longer_word : *)
val newTrie : trie

(* Question 16 : Fonction trie_most_used_word (Question N°4 de l'introduction)

   Cette fonction permet de connaitre le mot le plus utilisé et son nombre d'occurrences, dans un trie donné.

   trie_most_used_word : trie -> (word * int) = <fun>
   
   @param   tr    Le trie dans lequel chercher le mot le plus utilisé.
   @return  tuple Retourne un couple (word * int), word étant le mot le plus utilisé, et l'entier, son nombre d'occurences.
*)
val trie_most_used_word : trie -> (word * int)

(* Test de la fonction trie_most_used_word : *)
val newTrie : trie
val trieTest : trie
val newTrie : trie
val trieLA : trie

(* Question 16 : Fonction trie_search_words_n_letters (Question N°5 de l'introduction, partie 1/2)

   Cette fonction parcours le trie tr passé en paramètre, et retourne une liste de mots constitués d'au moins n lettres.
   
   trie_search_words_n_letters : trie -> int -> word list -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots d'au moins n lettres.
   @param   n                 Le nombre de lettres que le mot doit compter, pour faire partie de la liste que l'on renvoie.
   @param   wordsAlreadySeen  La liste de mots, qui nous permet d'accumuler les mots d'au moins n lettres, en parcourant l'arbre.
   @param   currentWord       Le mot courrant, incluant toutes les lettres rencontrées depuis la racine, jusqu'au noeud en cours.
   @return  word list         La liste de mots renvoyée à la fin, qui compte tous les mots constitués d'au moins n lettres.
*)
val trie_search_words_n_letters : trie -> int -> word list -> word -> word list

(* Question 16 : Fonction trie_words_more_n_letters (Question N°5 de l'introduction, partie 2/2)

   Cette fonction fait appel à la fonction définie juste avant, pour parcourir l'arbre, et récupérer les mots constitués d'au moins n lettres.
   
   trie_words_more_n_letters : trie -> int -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots d'au moins n lettres.
   @param   k                 Le nombre de lettres que le mot doit compter, pour faire partie de la liste que l'on renvoie.
   @return  word list         La liste de mots renvoyée à la fin, qui compte tous les mots constitués d'au moins n lettres.
*)
val trie_words_more_n_letters : trie -> int -> word list

(* Question 16 : Fonction trie_search_words_k_repeat (Question N°6 de l'introduction, partie 1/2)

   Cette fonction parcours le trie tr passé en paramètre, et retourne une liste de mots répétés au moins k fois.
   
   trie_search_words_k_repeat : trie -> int -> word list -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots répétés au moins k fois.
   @param   k                 Le nombre de fois que le mot doit être répété, pour faire partie de la liste que l'on renvoie.
   @param   wordsAlreadySeen  La liste de mots, qui nous permet d'accumuler les mots répétés plus de k fois, en parcourant l'arbre.
   @param   currentWord       Le mot courrant, incluant toutes les lettres rencontrées depuis la racine, jusqu'au noeud en cours.
   @return  word list         La liste de mots renvoyée à la fin, qui compte tous les mots répétés au moins k fois.
*)
val trie_search_words_k_repeat : trie -> int -> word list -> word -> word list

(* Question 16 : Fonction trie_words_k_repeat (Question N°6 de l'introduction, partie 2/2)

   Cette fonction fait appel à la fonction définie juste avant, pour parcourir l'arbre, et récupérer les mots répétés plus de k fois.
   
   trie_words_k_repeat : trie -> int -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots répétés au moins k fois.
   @param   k                 Le nombre de fois que le mot doit être répété, pour faire partie de la liste que l'on renvoie.
   @return  word list         Une liste de mots qui sont répétés au moins k fois.
*)
val trie_words_k_repeat : trie -> int -> word list

(* Question 16 : Fonction trie_search_words_with_prefix (Question N°7 de l'introduction, partie 1/2)

   Cette fonction parcours le trie tr passé en paramètre, et retourne une liste de mots dont le préfixe correspond à celui donné en paramètre.
   
   trie_search_words_with_prefix : trie -> word -> word list -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots dont le préfixe est le paramètre prefix.
   @param   prefix            Le préfixe qui doit composer le mot, pour que le mot fasse partie de la liste que l'on renvoie.
   @param   wordsAlreadySeen  La liste de mots, qui nous permet d'accumuler les mots composés du préfixe donné, en parcourant l'arbre.
   @param   currentWord       Le mot courrant, incluant toutes les lettres rencontrées depuis la racine, jusqu'au noeud en cours.
   @return  word list         La liste de mots renvoyée à la fin, qui compte tous les mots composés du préfixe donné en paramètre.
*)
val trie_search_words_with_prefix : trie -> word -> word list -> word -> word list

(* Question 16 : Fonction trie_words_with_prefix (Question N°7 de l'introduction, partie 2/2)

   Cette fonction fait appel à la fonction définie juste avant, pour parcourir l'arbre, et récupérer les mots commençant pas le préfixe donné.
   
   trie_words_with_prefix : trie -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots dont le préfixe est le paramètre prefix.
   @param   prefix            Le préfixe qui doit composer le mot, pour que le mot fasse partie de la liste que l'on renvoie.
   @return  word list         La liste de mots renvoyée à la fin, qui compte tous les mots composés du préfixe donné en paramètre.
*)
val trie_words_with_prefix : trie -> word -> word list
