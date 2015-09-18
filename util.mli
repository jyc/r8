val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val html_of_sxml : PpxSexp.sexp -> string
val string_of_char : char -> string
val string_of_atom : PpxSexp.sexp -> string
val string_of_sexp : PpxSexp.sexp -> string

val parts_of_path : string -> string list
