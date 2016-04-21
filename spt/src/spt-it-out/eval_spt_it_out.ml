(** Analyze s-experssions, conversion to plotting language and execution.

    @author eaburns
    @since 2010-05-14
*)

open Printf
open Geometry
open Verbosity
open Evaluate

let eval_display eval_rec env line operands =
  (** [eval_display eval_rec env line operands] evaluates a display
      function.  This function displays the plots(s) using GTK.  The
      result is the final plots that was displayed. *)
  List.fold_left
    (fun _ p -> match eval_rec env p with
       | (Num_by_num_plot p) as vl -> p#display; vl
       | (Num_by_nom_plot p) as vl -> p#display; vl
       | (Tree_plot p) as vl -> p#display; vl
       | x ->
	   printf "line %d: Expected plot, got %s\n"
	     (Sexpr.line_number p) (value_name x);
	   raise (Evaluate.Invalid_argument (Sexpr.line_number p)))
    unit_value operands

let help_str_display =
  "(display <plot>+)\n\
Displays the plots one-at-a-time using GTK.  The resulting value\n\
is the last plot that was displayed."


let eval_output eval_rec env line = function
    (** [eval_output eval_rec env line] evaluates an output function.
	This outputs the given plot to the specified file.  The result is
	the plot that was output. *)
  | Sexpr.String(_, filename) :: p :: [] ->
      begin match eval_rec env p with
	| (Num_by_num_plot p) as vl -> p#output filename; vl
	| (Num_by_nom_plot p) as vl -> p#output filename; vl
       | x ->
	   printf "line %d: Expected plot, got %s\n"
	     (Sexpr.line_number p) (value_name x);
	   raise (Evaluate.Invalid_argument (Sexpr.line_number p))
      end
  | x :: _ -> raise (Evaluate.Invalid_argument (Sexpr.line_number x))
  | [] -> raise (Evaluate.Invalid_argument line)


let help_str_output =
  "(output <string> <plot>)\nOutputs the given plot to the given file.\n\
The resulting value is the last plot that was output."


let rec functions () =
  [
    "display", eval_display, help_str_display;
    "output", eval_output, help_str_output;

    "help", eval_help,
    "(help <ident>)\nOutputs help information for a function";

    "quit", (fun _ _ _ _ -> raise End_of_file), "(quit)\nQuits";
  ]
  @ Evaluate.functions
  @ Options.functions
  @ Eval_data.functions
  @ Eval_num_by_num.functions
  @ Eval_num_by_nom.functions
  @ Eval_tree.functions

and eval_help eval_rec env line = function
  | Sexpr.Ident (lid, id) :: [] ->
      let fs = functions () in
	begin
	  try
	    let _, _, help_str =
	      List.find (fun (n, _, _) -> n = id) fs
	    in
	      printf "%s\n" help_str;
	      unit_value
	  with Not_found ->
	    printf "%s is not a function name, try one of:\n" id;
	    List.iter (fun (n, _, _) -> Printf.printf "\t%s\n" n) fs;
	    unit_value
	end
  | x :: _ ->
      printf "line %d: Expected identifier\n" (Sexpr.line_number x);
      raise (Invalid_argument (Sexpr.line_number x));
  | [] ->
      printf "line %d: Expected identifier\n" line;
      raise (Invalid_argument line)


let rec eval env expr = Evaluate.evaluate (functions ()) eval env expr
  (** [eval env expr] evaluates a spt-it-out expression. *)
