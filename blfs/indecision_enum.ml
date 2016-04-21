(** Enumerates a tree dumping information for each node that may be
    useful in debugging the indecision metric.

    @author eaburns
    @since 2010-12-13
*)

open Fn
open Printf

let interior_nodes = "interior nodes"

let output_kids costs bests discr indec depth =
  (** [output_kids costs bests discr indec depth] outputs an altrow
      for each child node.  The result is the value of the best leaf
      seen so far. *)
  let rank = ref 0 in
  let b_cost = costs.(0) in
  let b_leaf = Array.fold_left Math.fmin bests.(0) bests in
    assert ((Array.length costs) = (Array.length bests));
    Wrarray.iter2 (fun cost best ->
		     let discr = if !rank = 0 then discr else discr + 1 in
		     let indec' = cost -. b_cost in
		     let indec_sum = indec +. indec' in
		     let leaf_diff = best -. b_leaf in
		     let leaf_frac = best /. b_leaf in
		       Datafile.write_alt_row_prefix stdout interior_nodes;
		       printf "%d\t" depth;
		       printf "%d\t" !rank;
		       printf "%d\t" discr;
		       printf "%g\t" cost;
		       printf "%g\t" indec';
		       printf "%g\t" indec_sum;
		       printf "%g\t" best;
		       printf "%g\t" b_leaf;
		       printf "%g\t" leaf_diff;
		       printf "%g\n" leaf_frac;
		       incr rank)
      costs bests;
    b_leaf

exception Halt

let rec dfs leaf_cost child_costs info discr indec depth root =
  (** [dfs leaf_cost child_costs info discr indec depth root]
      performs a depth-first enumeration of the tree outputting a
      bunch of information about each internal node. *)
  if Info.halt_p info then raise Halt;
  if not (Info.leaf_p info root) then begin
    let nkids = Info.num_children info root in
    let costs = Array.of_list (child_costs root) in
    let kids = Array.init nkids (Info.get_child info root) in
    let bests =
      Array.mapi (fun i k ->
		    let indec' = indec +. (costs.(i) -. costs.(0)) in
		    let discr' = if i = 0 then discr else discr + 1 in
		    let depth' = depth + 1 in
		      dfs leaf_cost child_costs info discr' indec' depth' k)
	kids
    in
      begin try
	assert ((Array.length costs) = nkids);
	assert ((Array.length kids) = nkids);
	assert ((Array.length bests) = nkids);
      with _ ->
	printf "nkids=%d\n" nkids;
	printf "|costs|=%d\n" (Array.length costs);
	printf "|kids|=%d\n" (Array.length kids);
	printf "|bests|=%d\n" (Array.length bests);
      end;
      output_kids costs bests discr indec depth;
  end else begin
    ignore (Info.optimal_p info root);
    leaf_cost root
  end


let search ?(halt=[Info.Never]) ?(log=no_op2) ?(prev_best=None)
    ?(is_opt=constantly1 false) ?(prune=constantly2 false)
    copy leaf_cost is_better is_leaf nkids child_costs nth_kid root =
  let info =
    Info.make nkids nth_kid is_leaf is_better is_opt copy prune
      log prev_best halt
  in
    Datafile.write_alt_colnames stdout interior_nodes
      [ "depth"; "rank"; "discrepancies"; "heuristic"; "indecision";
	"indecision sum"; "best leaf"; "best sibling leaf"; "leaf diff";
	"leaf frac"; ];
    begin
      try
	dfs leaf_cost child_costs info 0 0. 0 root |> ignore;
      with Halt -> ()
    end;
    (Info.curr_best info), (Info.stats info), false, true
