(** Pattern databases for array-of-int domains.

    @author eaburns
    @since 2010-01-25
*)

type t = {
  name : string;
  (* Domain-specific PDB name.  This takes care of the case where
     there may be many PDBs of the same size for a domain. *)

  ground_size : int;
  (* Number of elements in an array in a ground-level state. *)

  pdb_size : int;
  (* Number of elements in an array in the abstract-level state. *)

  tbl: float array;
  (* Table of heuristic values. *)

  max_cost : float;
  (* The maximum heuristic value in the table. *)
}


let positions_of nums s =
  (** [positions_of nums s] gets the positions of the numbers in the
      [nums] array in the int-vector [s].  The positions are in the
      order they are given in [nums].  This is O(n^2) and kind of
      sucks. *)
  let n = Array.length s in
  let n' = Array.length nums in
  let res = Array.make n' ~-1 in
  let i = ref 0
  and j = ref 0
  and k = ref 0 in
  let found = ref false in
    while !i < n && !k < n' do
      j := 0;
      found := false;
      while (!j < n') && not !found do
	if nums.(!j) = s.(!i)
	then begin
	  res.(!j) <- !i;
	  found := true;
	  incr k;
	end else
	  incr j;
      done;
      incr i;
    done;
    res


let abst_nodes n abst_size =
  (** [abst_nodes n abst_size] gets the number of abstract nodes in a
      pattern database that abstracts all except for [abst_size]
      elements of an [n] element array. *)
  let rec get_n_nodes left =
    if left < 0
    then 1
    else (n - left) * (get_n_nodes (left - 1))
  in get_n_nodes (abst_size - 1)


let make name expand ground_size goal phi =
  (** [make name expand ground_size goal] makes a new pattern database
      by uniform-cost search in reverse from a goal state.

      [name] is the domain-specific pattern database name.

      [expand] expands a (state * cost) tuple giving a set of (child *
      cost) tuples

      [ground_size] is the size (in elements) of a ground state.

      [goal] is the goal state which is used to begin the search.
  *)
  let pdb_size = Array.length (phi goal) in
  let rank = Array_hasher.lex_rank_abst ground_size in
  let goal_rank = rank (phi goal) in
  let nabst_nodes = abst_nodes ground_size pdb_size in
    Verb.pr Verb.toplvl "domain size:    %d\n%!" ground_size;
    Verb.pr Verb.toplvl "abstract size:  %d\n%!" pdb_size;
    Verb.pr Verb.toplvl "abstract nodes: %d\n%!" nabst_nodes;
    Verb.pr Verb.toplvl "goal rank:      %d\n%!" goal_rank;
    let tbl = Array.make nabst_nodes infinity in
    let goal_entry = goal, 0. in
    let o = Dpq.create_with (fun (_, a) (_, b) -> a < b) goal_entry in
    let count = ref 0 in
      Dpq.insert o goal_entry;
      tbl.(goal_rank) <- 0.;
      while not (Dpq.empty_p o) do
	let node, cost = Dpq.extract_first o in
	  List.iter (fun (child, child_cost) ->
		       let child_rank = rank (phi child) in
		       let old_cost = tbl.(child_rank) in
			 if old_cost > child_cost
			 then begin
			   if not (Math.finite_p old_cost)
			   then begin
			     incr count;
			     if Verb.level Verb.debug
			       && (Math.divisible !count (nabst_nodes / 100))
			     then
			       Wrutils.pr "%3d%% complete\n%!"
				 ((!count * 100) / nabst_nodes)
			   end;
			   tbl.(child_rank) <- child_cost;
			   Dpq.insert o (child, child_cost)
			 end)
	    (expand node cost)
      done;
      Verb.pr Verb.toplvl "PDB generation complete\n%!";
      Verb.pr Verb.toplvl "abstract nodes seen: %d\n%!" !count;
      let max_cost = Array.fold_left (Math.fmax) neg_infinity tbl in
	Verb.pr Verb.toplvl "maximum cost:        %f\n%!" max_cost;
	{
	  name = name;
	  ground_size = ground_size;
	  pdb_size = pdb_size;
	  tbl = tbl;
	  max_cost = max_cost;
	}


let lookup pdb phi s =
  (** [lookup pdb phi s] looks up the heuristic value for [s] using
      abstraction function [phi] with the given pattern database. *)
  let rank = Array_hasher.lex_rank_abst pdb.ground_size (phi s) in
    pdb.tbl.(rank)
