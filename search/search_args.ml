(** A set of checks for search arguments *)

let args_to_str args =
  Array.fold_left (fun accum e ->
		     Wrutils.str "%s %s" accum e) "" args


let is_empty alg_name args =
  if args <> [||]
  then
    failwith (Wrutils.str "%s: expected no arguments, but got%s"
		alg_name (args_to_str args))


let get_t t_of_string t_name alg_name args index =
  try
    t_of_string args.(index)
  with
    | Invalid_argument _ ->
	failwith (Wrutils.str "%s: index %i out of bounds" alg_name index)
    | _ -> failwith (Wrutils.str "%s: couldn't convert %s to %s"
		       alg_name args.(index) t_name)


let get_t_array t_of_string alg_name args =
  try
    Array.map t_of_string args
  with _ -> failwith (Wrutils.str "%s: couldn't create array" alg_name)



let get_float = get_t float_of_string "float"

let get_int = get_t int_of_string "int"

let get_bool = get_t bool_of_string "bool"

let get_string =
  (fun alg_name args index ->
     try
       args.(index)
     with
       | Invalid_argument _ ->
	   failwith (Wrutils.str "%s: index %i out of bounds" alg_name index))

let get_float_array = get_t_array float_of_string

let get_int_array = get_t_array int_of_string

(* EOF *)
