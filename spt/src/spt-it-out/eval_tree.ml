(** Evaluation of expressions for creating tree plots.

    @author eaburns
    @since 2010-07-05
*)

open Printf
open Evaluate


let rec tree_node line = function
  | Color c
  | List [| Color c |] ->
      { Tree_vis.color = c ; Tree_vis.succs = [||] }
  | List [| Color c; List succs; |] ->
      { Tree_vis.color = c ; Tree_vis.succs = Array.map (tree_node line) succs }
  | _ ->
      printf "line %d: expected tree node\n" line;
      raise (Invalid_argument line)


let help_str_tree_node =
"(tree-node :color <color> [:succs (<tree node>+)]) makes a tree node"



let eval_sunburst_tree_plot eval_rec env line operands =
  (** [eval_sunburst_tree_plot eval_rec env line operands] evaluates a
      sunburst style tree plot. *)
  let module S = Sexpr in
  let title = ref None in
  let outlined = ref None in
  let tree = ref None in
  let opt_spec = [
    Options.string_option_ref ":title" title;
    Options.bool_option_ref ":outlined" outlined;
    ":tree", Options.List (fun l lst -> tree := Some (tree_node l (List lst)));
  ] in
    Options.handle eval_rec env opt_spec operands;
    match !tree with
      | Some root ->
	  let style = match !outlined with
	    | Some true -> Tree_vis.Sunburst.outlined_style
	    | _ -> Tree_vis.Sunburst.default_style
	  in
	  Tree_plot (Tree_vis.plot ?title:!title style root)
      | _ ->
	  printf "line %d: expected a tree node" line;
	  raise (Invalid_argument line)



let help_str_sunburst_tree_plot =
  "(sunburst-tree-plot :tree <tree>) makes a sunburst style tree plot."


let functions = [
  "sunburst-tree-plot", eval_sunburst_tree_plot, help_str_sunburst_tree_plot;
]
