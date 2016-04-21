(**

    @author eaburns
    @since 2010-06-19
*)

open Geometry
open Printf
open Verbosity

type value =
  | List of value array
  | Bool of bool
  | Number of float
  | String of string
  | Color of Drawing.color
  | Font of Drawing.text_style
  | Length of Length.t
  | Legend_loc of Legend.location
  | Num_by_num_dataset of Num_by_num.dataset_type
  | Num_by_num_plot of Num_by_num.plot_type
  | Num_by_nom_dataset of Num_by_nom.dataset_type
  | Num_by_nom_plot of Num_by_nom.plot_type
  | Tree_plot of Tree_vis.plot_type


type environment = {
  bindings : (string * value) list;
  next_glyph : unit -> Drawing.glyph;
  next_dash : unit -> Length.t array;
  next_line_errbar : unit -> Num_by_num.line_errbar_style;
  next_fill : unit -> Drawing.fill_pattern;
}


exception Invalid_argument of int

let init_environment =
  let next_dash = Factories.default_dash_factory () in
    {
      bindings = [];
      next_glyph = Factories.default_glyph_factory ();
      next_dash = next_dash;
      next_line_errbar = Num_by_num.line_errbar_factory next_dash ();
      next_fill = Factories.default_fill_pattern_factory ();
    }


let lookup_ident env l id =
  try List.assoc id env.bindings
  with Not_found -> failwith (sprintf "line %d: Unknown identifier: %s" l id)


let is_function funcs id = List.exists (fun (n, _, _) -> n = id) funcs


let value_name = function
    (** [to_string t] gets the string representation. *)
  | List _ -> "List"
  | Bool _ -> "Bool"
  | Number _ -> "Number"
  | String _ -> "String"
  | Length _ -> "Length"
  | Legend_loc _ -> "Legend_loc"
  | Color _ -> "Color"
  | Font _ -> "Font"
  | Num_by_num_dataset _ -> "Num_by_num_dataset"
  | Num_by_num_plot _ -> "Num_by_num_plot"
  | Num_by_nom_dataset _ -> "Num_by_nom_dataset"
  | Num_by_nom_plot _ -> "Num_by_nom_plot"
  | Tree_plot _ -> "Tree_plot"


let rec string_of_value = function
  | Bool b -> string_of_bool b
  | Number n -> sprintf "%g" n
  | String s -> sprintf "%s" s
  | Length l -> Length.to_string l
  | Legend_loc l -> begin match l with
      | Legend.At (_, _, _) -> "legend-at"
      | Legend.Upper_left -> ":upper-left"
      | Legend.Upper_right -> ":upper-right"
      | Legend.Lower_left -> ":lower-left"
      | Legend.Lower_right -> ":lower-right"
    end
  | Color c -> Drawing.string_of_color c
  | Font f -> f.Drawing.text_font
  | List l ->
      (Array.fold_left
	 (fun str v -> sprintf "%s %s" str (string_of_value v)) "(" l)
      ^ " )"
  | Num_by_num_dataset _ -> "<Num_by_num_dataset>"
  | Num_by_num_plot _ -> "<Num_by_num_plot>"
  | Num_by_nom_dataset _ -> "<Num_by_nom_dataset>"
  | Num_by_nom_plot _ -> "<Num_by_nom_plot>"
  | Tree_plot _ -> "<Tree_plot>"


let unit_value = List [||]


let evaluate functs eval_rec env = function
    (** [evaluate functs eval_rec env expr] evaluates [expr].
	[functs] is a list of function names followed by their
	evaluation function.  [eval_rec] is the evaluation function to
	call when recursive evaluation is required. *)
  | Sexpr.List (lline, (Sexpr.Ident (line, func_name)) :: operands)
      when (is_function functs func_name) ->
      let _, f, help_str =
	List.find (fun (n, _, _) -> n = func_name) functs
      in
	begin
	  try f eval_rec env line operands
	  with Invalid_argument line ->
	    vprintf verb_normal "%s\n" help_str;
	    failwith (sprintf "line %d: Invalid arguments to %s\n"
			line func_name)
	end
  | Sexpr.Number (_, vl) -> Number vl
  | Sexpr.String (_, n) -> String n
  | Sexpr.Ident (l, "true") -> Bool true
  | Sexpr.Ident (l, "false") -> Bool false
  | Sexpr.Ident (l, id) -> lookup_ident env l id
  | Sexpr.List (l, exps) ->
      let n = List.length exps in
      let ary = Array.create n unit_value in
      let i = ref 0 in
	List.iter (fun e -> ary.(!i) <- eval_rec env e; incr i) exps;
	List ary


(** {1 Default functions} *****************************)


let eval_let eval_rec env line = function
    (** [eval_let eval_rec env line operands] evaluates the operands
	of a let expression. *)
  | Sexpr.List (_, bindings) :: (Sexpr.List (_, _) as expr) :: [] ->
      let binds =
	List.fold_left
	  (fun lst b ->
	     match b with
	       | Sexpr.List (_, Sexpr.Ident (_, name) :: value :: []) ->
		   (name, eval_rec env value) :: lst
	       | e ->
		   failwith (sprintf "line %d: Malformed binding"
			       (Sexpr.line_number e)))
	  [] bindings
      in
	eval_rec {env with bindings = (binds @ env.bindings)} expr
  | e :: _ ->
      printf "line %d: Malformed let statement\n" line;
      raise (Invalid_argument line)
  | [] ->
      printf "line %d: let expression: Unexpected end of file\n" line;
      raise (Invalid_argument line)


let help_str_let =
  "(let ([(<ident#> <expr#>)]+) (<expr>))\n\
Binds the value of <expr#> to the identifier <ident#> and then \n\
evaluates <expr> with the given set of bindings.  The result is \n\
the result of <expr>."


let eval_letstar eval_rec env line = function
    (** [eval_letstar eval_rec env line operands] evaluates the
	operands of a let* expression. *)
  | Sexpr.List (_, bindings) :: (Sexpr.List (_, _) as expr) :: [] ->
      let env' =
	List.fold_left
	  (fun env' b ->
	     match b with
	       | Sexpr.List (_, Sexpr.Ident (_, name) :: value :: []) ->
		   let b = name, eval_rec env' value in
		     {env with bindings =  b :: env'.bindings}
	       | e ->
		   failwith (sprintf "line %d: Malformed binding"
			       (Sexpr.line_number e)))
	  env bindings
      in
	eval_rec env' expr
  | e :: _ ->
      printf "line %d: Malformed let* statement\n" line;
      raise (Invalid_argument line)
  | [] ->
      printf "line %d: let* expression: Unexpected end of file\n" line;
      raise (Invalid_argument line)


let help_str_letstar =
  "(let* ([(<ident#> <expr#>)]+) (<expr>))\n\
The same as a let function except that expressions in the binding\n\
list can refer to previous bindings in the same list."


let eval_print eval_rec env line operands =
  (** [eval_print eval_rec env line operands] evaluates a print
      expression. *)
  List.fold_left
    (fun _ e ->
       let v = eval_rec env e in
	 printf "%s" (string_of_value v);
	 v)
    unit_value operands


let help_str_print =
  "(print <expr>+)\n\
Prints the 'value' of the expressions.  This works for simple \n\
expressions, but more complex experessions may not print much \n\
information."


let functions = [
  "let", eval_let, help_str_let;
  "let*", eval_letstar, help_str_letstar;
  "print", eval_print, help_str_print;
]
