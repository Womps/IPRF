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