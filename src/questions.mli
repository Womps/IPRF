(* Romain EPIARD

   Programmation Fonctionnelle (IPRF)
   Projet : Analyse d'un fichier texte en OCaml.

   Interface du module questions.
*)
type word

(* D�finition de diff�rentes variables, qui nous permettront de r�aliser divers tests : *)
val hello : word
val car1 : char
val car2 : char
val car3 : char
val car4 : char
val car5 : char

(* Question 1 : Fonction is_a_letter

   Cette fonction permet de savoir si le caract�re donn� en entr�e est bien une lettre valide (lettre majuscule, minuscule, tiret et apostrophe)

   is_a_letter : char -> bool = <fun>

   @param  c        Le caract�re � analyser.
   @return bool�en  True si le caract�re est une lettre valide, sinon false.
*)
val is_a_letter : char -> bool

(* Question 2 : Fonction is_valid

   La fonction is_valid w permet de connaitre la validit� d'un mot, par rapport � l'alphabet que nous avons constitu� gr�ce � la fonction is_a_letter.

   is_valid : word -> bool = <fun>

   @param  w        Le mot contenant les caract�res � �valuer.
   @return bool�en  True si le mot est non vide et constitu� de lettres valides, sinon false.
*)
val is_valid : word -> bool

(* Question 3 : Fonction to_lower

   Cette fonction permet de passer toutes les lettres d'un mot en minuscule. Celles qui �taient en majuscule, passeront en minuscule. Celles d�j� en minuscule, le resteront.

   to_lower : word -> word = <fun>

   @param  w     Le mot � rendre en minuscule.
   @return word  Le mot pass� en minuscule.
*)
val to_lower : word -> word

(* Question 4 : Fonction first_valid_char (Partie 1/2)

   La fonction first_valid_char parcourt un mot w. Elle s'arr�te et retourne le mot w, d�s qu'elle trouve une lettre valide.

   first_valid_char : word -> word = <fun>

   @param  w     Le mot � parcourir.
   @return word  Retourne un mot contenant la premi�re lettre valide trouv�e, ainsi que la suite du mot.
*)
val first_valid_char : word -> word

(* Question 4 : Fonction trim (Partie 2/2)

   La fonction trim utilise la fonction first_valid_char. Comme la fonction first_valid_char retourne la premi�re lettre valide, avec la suite du mot, on lui passe notre mot � l'envers, afin de partir de la fin, pour s'arr�ter d�s que l'on croise une lettre valide, et ainsi supprimer toutes les lettres du mot, apr�s la derni�re lettre valide. Le retour de first_valid_char est de nouveau invers�, pour remettre le mot � l'endroit.

   trim : word -> word = <fun>

   @param  w     Le mot � traiter.
   @return word  Retourne le mot avec les caract�res, suivant la derni�re lettre valide, supprim�s.
*)
val trim : word -> word

(* D�finition de quelques variables, qui nous permettront de r�aliser divers tests : *)
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

(* === PARTIE 2 : R�CUP�RATION DE LA LISTE DES MOTS D'UN FICHIER TEXTE ===

   Question 5 : Fonction print_word (Partie 1/2)

   Cette fonction est utilis�e pour afficher le mot contenu dans la liste en entr�e. 

   print_word : word -> unit = <fun>

   @param  w     Le mot � afficher.
   @return unit  Ne retourne rien.
*)
val print_word : word -> unit

(* Question 5 : Fonction print_text (Partie 2/2)

   Cette fonction permet d'afficher chaque mot contenu dans une liste de mots. Cela nous permet ainsi de tester le code fourni et la fonction print_word.

   print_text : word list -> unit = <fun>
   
   @param  l     Liste de liste. On a une liste de mots en entr�e.
   @return unit  Ne retourne rien.
*)
val print_text : word list -> unit

(* Test de la fonction print_text : 
   On commence par r�cup�rer la liste de mots contenue dans le fichier nomm� fichier.txt.
*)
val text : word list

(* Question 7 : Fonction nub

   La fonction nub prend une liste en entr�e, et retourne une liste, avec les doublons en moins. Pour ce faire, on utilise la fonction List.mem, pour v�rifier si l'�l�ment sur lequel on est, existe dans la liste. On aurait aussi pu impl�menter une fonction de recherche. Mais afin de gagner du temps, on utilise la fonction pr�vue dans le module List.

   nub : 'a list -> 'a list = <fun>

   @param   l       La liste, dans laquelle il faut retirer les doublons.
   @return 'a list  Retourne la liste donn�e en entr�e, avec les doublons en moins.
*)
val nub : 'a list -> 'a list

(* Question 8 : Fonction count_words

   La fonction count_words r�cup�re une liste de mots dans un fichier, sur la donn�e d'un chemin vers un fichier texte. On aurait pu utiliser des List.fold_left, ou faire des fonctions auxiliaires pour compter le nombre de mots valides et de mots valides diff�rents. Comme la liste de mots issue de get_words est une liste de mots valides uniquement (il n'y a pas de mots invalides, en sortie de cette fonction), nous pouvons directement utiliser List.length, sans avoir besoin de reparcourir la liste, et de v�rifier tous les mots uns par uns.

   Retourne un couple (m, n) o� :
           * m est le nombre de mots valides dans le fichier,
           * n est le nombre de mots valides diff�rents dans le fichier.

   count_words : string -> (int * int) = <fun>

   @param   file   Le chemin vers un fichier contenant du texte.
   @return  tuple  Retourne un couple d'entiers (m, n).
*)
val count_words : string -> (int * int)

(* === PARTIE 3 : LA STRUCTURE DE TRIE === *)
type trie

(* D�claration d'un Trie vide : *)
val empty_trie : trie

(* Construction du Trie de la figure 1, de fa�on non efficace : *)
val ns : trie
val ne : trie
val na : trie
val nl : trie
val nn : trie
val nu : trie
val example : trie

(* Question 10 : Fonction trie_get 
   
   La fonction trie_get prend un mot w et un trie t en entr�e, pour renvoyer la valeur associ�e � w, dans t. Si le mot w ne peut pas �tre retrouv� dans l'arbre (aucun fils ne correspond � la lettre que l'on cherche � lire), alors on renvoie la valeur 0.

   trie_get : word -> trie -> int = <fun>

   @param  w    Le mot � chercher dans l'arbre.
   @param  t    L'arbre t dans lequel chercher le mot w.
   @return int  La valeur associ�e au mot w, dans l'arbre t.
*)
val trie_get : word -> trie -> int

(* Question 11 : Fonction trie_incr 

   La fonction trie_incr prend un mot w et un trie tr en entr�e, pour renvoyer un nouveau trie tr' dans lequel la valeur associ�e � w a �t� augment�e de 1.

   trie_incr : word -> trie -> trie = <fun>

   @param  w    Le mot pour lequel on veut incr�menter de 1 la valeur dans l'arbre.
   @param  t    L'arbre dans lequel on veut incr�menter la valeur de w.
   @return trie L'arbre avec la nouvelle valeur associ�e � w.
*)
val trie_incr : word -> trie -> trie

(* Test de la fonction trie_incr : *)
val newTrie : trie

(* Question 12 : Fonction trie_construction

   Cette fonction va nous permettre de parcourir la liste de mots text en entr�e, et d'ajouter chaque mots � l'arbre tr.
   
   trie_construction : word list -> trie = <fun>
   
   @param  text La liste de mots � ajouter � l'arbre tr.
   @return trie L'arbre construit � partir de la liste de mots.
*)
val trie_build : trie -> word -> in_channel -> trie

(* Question 12 : Fonction trie_words

   Fonction qui prend en entr�e un chemin vers un fichier, pour renvoyer le trie construit � partir des mots de ce fichier.
   
   trie_words : string -> trie = <fun>
   
   @param  s    La cha�ne de caract�res correspondant au chemin du fichier � lire.
   @return trie L'arbre construite � partir des mots du fichier donn� en entr�e.
*)
val trie_words : string -> trie

(* Test de la fonction trie_words : *)
val test_trie_words : trie

(* Question 13 : Fonction trie_card

   Cette fonction compte le nombre de noeuds qui ont une valeur non nulle dans un trie donn�.

   trie_card : trie -> int = <fun>
   
   @param   tr   L'arbre pour lequel on doit compter les noeuds avec une valeur non nulle.
   @return  int  Le nombre entier, correspondant au cardinal des noeuds avec une valeur non nulle dans l'arbre tr.
*)
val trie_card : trie -> int

(* Question 14 : Fonction trie_sum

   Cette fonction retourne la somme des valeurs contenues dans les noeuds d'un trie donn�.

   trie_sum : trie -> int = <fun>
   
   @param   tr   L'arbre pour lequel on doit faire la somme des valeurs contenues dans le trie.
   @return  int  Le nombre entier, correspondant � la somme des valeurs contenues dans le trie.
*)
val trie_sum : trie -> int

(* Question 15 : Fonction count_words_v2 
   
   La fonction count_words_v2 retourne la m�me chose que count_words mais en utilisant les trie que nous venons d'impl�menter, au lieu de listes.

   Retourne un couple (m, n) o� :
	   * m est le nombre de mots valides dans le fichier,
	   * n est le nombre de mots valides diff�rents dans le fichier.
   
   count_words_v2 : string -> (int * int) = <fun>
   
   @param   f     Le fichier � lire en entr�e.
   @return  tuple Retourne un couple d'entiers (m, n).
*)
val count_words_v2 : string -> (int * int)

(* Question 16 : Fonction trie_longer_word (Question N�3 de l'introduction)

   Cette fonction retourne un couple, le premier �l�ment �tant le mot le plus long dans l'arbre. Le deuxi�me �tant sa taille.
   
   trie_longer_word : trie -> (word * int) = <fun>

   @param   tr    Le trie dans lequel chercher le mot le plus long.
   @return  tuple Retourne un couple (word * int), word �tant le mot trouv�, et l'entier, sa taille.
*)
val trie_longer_word : trie -> (word * int)

(* Test de la fonction trie_longer_word : *)
val newTrie : trie

(* Question 16 : Fonction trie_most_used_word (Question N�4 de l'introduction)

   Cette fonction permet de connaitre le mot le plus utilis� et son nombre d'occurrences, dans un trie donn�.

   trie_most_used_word : trie -> (word * int) = <fun>
   
   @param   tr    Le trie dans lequel chercher le mot le plus utilis�.
   @return  tuple Retourne un couple (word * int), word �tant le mot le plus utilis�, et l'entier, son nombre d'occurences.
*)
val trie_most_used_word : trie -> (word * int)

(* Test de la fonction trie_most_used_word : *)
val newTrie : trie
val trieTest : trie
val newTrie : trie
val trieLA : trie

(* Question 16 : Fonction trie_search_words_n_letters (Question N�5 de l'introduction, partie 1/2)

   Cette fonction parcours le trie tr pass� en param�tre, et retourne une liste de mots constitu�s d'au moins n lettres.
   
   trie_search_words_n_letters : trie -> int -> word list -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots d'au moins n lettres.
   @param   n                 Le nombre de lettres que le mot doit compter, pour faire partie de la liste que l'on renvoie.
   @param   wordsAlreadySeen  La liste de mots, qui nous permet d'accumuler les mots d'au moins n lettres, en parcourant l'arbre.
   @param   currentWord       Le mot courrant, incluant toutes les lettres rencontr�es depuis la racine, jusqu'au noeud en cours.
   @return  word list         La liste de mots renvoy�e � la fin, qui compte tous les mots constitu�s d'au moins n lettres.
*)
val trie_search_words_n_letters : trie -> int -> word list -> word -> word list

(* Question 16 : Fonction trie_words_more_n_letters (Question N�5 de l'introduction, partie 2/2)

   Cette fonction fait appel � la fonction d�finie juste avant, pour parcourir l'arbre, et r�cup�rer les mots constitu�s d'au moins n lettres.
   
   trie_words_more_n_letters : trie -> int -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots d'au moins n lettres.
   @param   k                 Le nombre de lettres que le mot doit compter, pour faire partie de la liste que l'on renvoie.
   @return  word list         La liste de mots renvoy�e � la fin, qui compte tous les mots constitu�s d'au moins n lettres.
*)
val trie_words_more_n_letters : trie -> int -> word list

(* Question 16 : Fonction trie_search_words_k_repeat (Question N�6 de l'introduction, partie 1/2)

   Cette fonction parcours le trie tr pass� en param�tre, et retourne une liste de mots r�p�t�s au moins k fois.
   
   trie_search_words_k_repeat : trie -> int -> word list -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots r�p�t�s au moins k fois.
   @param   k                 Le nombre de fois que le mot doit �tre r�p�t�, pour faire partie de la liste que l'on renvoie.
   @param   wordsAlreadySeen  La liste de mots, qui nous permet d'accumuler les mots r�p�t�s plus de k fois, en parcourant l'arbre.
   @param   currentWord       Le mot courrant, incluant toutes les lettres rencontr�es depuis la racine, jusqu'au noeud en cours.
   @return  word list         La liste de mots renvoy�e � la fin, qui compte tous les mots r�p�t�s au moins k fois.
*)
val trie_search_words_k_repeat : trie -> int -> word list -> word -> word list

(* Question 16 : Fonction trie_words_k_repeat (Question N�6 de l'introduction, partie 2/2)

   Cette fonction fait appel � la fonction d�finie juste avant, pour parcourir l'arbre, et r�cup�rer les mots r�p�t�s plus de k fois.
   
   trie_words_k_repeat : trie -> int -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots r�p�t�s au moins k fois.
   @param   k                 Le nombre de fois que le mot doit �tre r�p�t�, pour faire partie de la liste que l'on renvoie.
   @return  word list         Une liste de mots qui sont r�p�t�s au moins k fois.
*)
val trie_words_k_repeat : trie -> int -> word list

(* Question 16 : Fonction trie_search_words_with_prefix (Question N�7 de l'introduction, partie 1/2)

   Cette fonction parcours le trie tr pass� en param�tre, et retourne une liste de mots dont le pr�fixe correspond � celui donn� en param�tre.
   
   trie_search_words_with_prefix : trie -> word -> word list -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots dont le pr�fixe est le param�tre prefix.
   @param   prefix            Le pr�fixe qui doit composer le mot, pour que le mot fasse partie de la liste que l'on renvoie.
   @param   wordsAlreadySeen  La liste de mots, qui nous permet d'accumuler les mots compos�s du pr�fixe donn�, en parcourant l'arbre.
   @param   currentWord       Le mot courrant, incluant toutes les lettres rencontr�es depuis la racine, jusqu'au noeud en cours.
   @return  word list         La liste de mots renvoy�e � la fin, qui compte tous les mots compos�s du pr�fixe donn� en param�tre.
*)
val trie_search_words_with_prefix : trie -> word -> word list -> word -> word list

(* Question 16 : Fonction trie_words_with_prefix (Question N�7 de l'introduction, partie 2/2)

   Cette fonction fait appel � la fonction d�finie juste avant, pour parcourir l'arbre, et r�cup�rer les mots commen�ant pas le pr�fixe donn�.
   
   trie_words_with_prefix : trie -> word -> word list = <fun>
   
   @param   tr                Le trie dans lequel chercher les mots dont le pr�fixe est le param�tre prefix.
   @param   prefix            Le pr�fixe qui doit composer le mot, pour que le mot fasse partie de la liste que l'on renvoie.
   @return  word list         La liste de mots renvoy�e � la fin, qui compte tous les mots compos�s du pr�fixe donn� en param�tre.
*)
val trie_words_with_prefix : trie -> word -> word list
