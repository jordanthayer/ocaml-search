(* char utility code *)

      
let white_p = function
    ' ' | '\t' | '\n' | '\012' | '\r' -> true
  | _ -> false


let non_white_p c =
  not (white_p c)


let is_digit = function
    '0'..'9' -> true
  | _ -> false


let is_letter = function
    'a'..'z' | 'A'..'Z' -> true
  | _ -> false


let is_capital = function
    'A'..'Z' -> true
  | _ -> false
      

let int = function
    '0'..'9' as c ->
      (int_of_char c) - (int_of_char '0')
  | c -> invalid_arg "int_of_char"


let random () =
  char_of_int (Random.int 256)
    
    
(* EOF *)
