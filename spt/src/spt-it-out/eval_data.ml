(** Evaluating data.

    @author eaburns
    @since 2010-06-19
*)

open Geometry
open Printf
open Evaluate

let read_floats inch =
  (** [read_floats inch] reads the floats from the given channel. *)
  let floats = ref [] in
    try (while true do
	   let p = Scanf.fscanf inch " %f" (fun x -> Number x) in
	     floats := p :: !floats
	 done;
	 failwith "Impossible")
    with End_of_file ->
      List (Array.of_list (List.rev !floats))


(** {1 Convenience} ****************************************)

let scalars eval_rec env line lst =
  Array.map (function
	       | Number n -> n
	       | x -> failwith (sprintf "line %d: Expected number, got %s"
				  line (value_name x)))
    lst


let points eval_rec env line lst =
    Array.map (function
		 | List [| Number x; Number y |] -> point x y
		 | x -> failwith (sprintf "line %d: Expected point, got %s"
				    line (value_name x)))
      lst


let rec triples eval_rec env line lst =
  Array.map (function
	       | List [| Number i; Number j; Number k |] -> triple i j k
	       | x -> failwith (sprintf "line %d: Expected triple, got %s"
				  line (value_name x)))
    lst


let labeled eval_data eval_rec env line lst =
  let label, data = match lst with
    | [| String s; List data; |] -> Some s, data
    | data -> begin match data.(0) with
	| String s -> Some s, Array.sub data 1 ((Array.length data) - 1)
	| _ -> None, data
      end
  in
    label, eval_data eval_rec env line data

(** {1 Functions} ****************************************)


let eval_data_file eval_rec env line = function
    (** [eval_data_file eval_rec env line operands] evaluates a
	scalar file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let floats = read_floats inch in
	close_in inch;
	floats
  | _ ->
      printf "line %d: Malformed data-file expression" line;
      raise (Evaluate.Invalid_argument line)


let help_str_data_file =
  "(data-file <string>)\n\
Opens the given file and reads in floating point numbers in ASCII\n\
format."


let eval_data_cmd eval_rec env line = function
    (** [eval_data_cmd eval_rec env line operands] evaluates a
	scalar command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let floats = read_floats inch in
	ignore (Unix.close_process_in inch);
	floats
  | _ ->
      printf "line %d: Malformed data-cmd expression" line;
      raise (Evaluate.Invalid_argument line)


let help_str_data_cmd =
  "(data-cmd <string>)\n\
Evaluates the given command-line using a shell.  The output\n\
of the command is read as a list of floating point numbers."


let eval_group eval_rec env line operands =
  let rec get_n n accum = function
    | [] ->
	printf "line %d: List is not groupable by %d\n" line n;
	raise (Invalid_argument line)
    | hd :: tl ->
	let accum' = hd :: accum in
	  if (List.length accum') = n
	  then List.rev accum', tl
	  else get_n n accum' tl
  in
  let do_group n ary =
    let lst = ref (Array.to_list ary) in
    let groups = ref [] in
      while !lst <> [] do
	let group, rest = get_n n [] !lst in
	  groups := (List (Array.of_list group)) :: !groups;
	  lst := rest
      done;
      List (Array.of_list (List.rev !groups))
 in
  match operands with
    | Sexpr.Number(ln, n) :: expr :: [] ->
	begin match eval_rec env expr with
	  | List l ->
	      let n = truncate n in
		if n >= 2
		then do_group n l
		else begin
		  printf "line %d: Groups must be >= 2" ln;
		  raise (Invalid_argument ln);
		end
	  | x ->
	      printf "line %d: Expected a list, got %s\n"
		(Sexpr.line_number expr) (value_name x);
	      raise (Invalid_argument line)
	end;
    | x -> raise (Invalid_argument line)


let help_str_group =
  "(group <number> <expr>)\n\
Groups the values in the list resulting from the expression into\n\
a list of n-lists each of the size by the given number.  Groups must\n\
be at least 2 elemens."


let eval_log10 eval_rec env line operands =
  let rec do_log10 line = function
    | Number n -> Number (log10 n)
    | List l -> List (Array.map (do_log10 line) l)
    | x ->
	printf
	  "line %d: Expected: Number, Scalars, Points or Triples, got %s\n"
	  line (value_name x);
	raise (Invalid_argument line)
  in
    match operands with
      | e :: [] -> do_log10 (Sexpr.line_number e) (eval_rec env e)
      | es ->
	  List (Array.of_list
		  (List.map (fun e ->
			       do_log10 (Sexpr.line_number e)
				 (eval_rec env e)) es))


let help_str_log10 =
  "(log10 [<number>|<scalars>|<points>|<triples>])\n\
Gets the log base 10 of the values.  If the argument is a composite\n\
(points or triples) then the log is applied to all of the fields."


let eval_mean eval_rec env line = function
    (** [eval_mean eval_rec env line operands] computes the mean value
	of the single argument that is a list of scalars. *)
  | e :: [] ->
      begin match eval_rec env e with
	| List nums ->
	    let sum =
	      Array.fold_left
		(fun s -> function
		   | Number n -> s +. n
		   | _ ->
		       printf "line %d: Expects a list of numbers\n" line;
		       raise (Invalid_argument line))
		0. nums
	    in Number (sum /. (float (Array.length nums)))
	| _ ->
	    printf "line %d: Expects a list of numbers\n" line;
	    raise (Invalid_argument line)
      end
  | x :: _ -> raise (Invalid_argument (Sexpr.line_number x))
  | [] -> raise (Invalid_argument line)


let help_str_mean =
  "(mean (<numbers>))\nComuputes the mean of a list of numbers."



let functions = [
  "data-file", eval_data_file, help_str_data_file;
  "data-cmd", eval_data_cmd, help_str_data_cmd;
  "group", eval_group, help_str_group;
  "log10", eval_log10, help_str_log10;
  "mean", eval_mean, help_str_mean;
]
