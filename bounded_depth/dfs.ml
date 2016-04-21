(* $Id: dfs.ml,v 1.1 2003/07/17 19:12:20 ruml Exp ruml $

   depth-first search
*)

exception Halted

type stats ={
  mutable expand : int;
  mutable gen: int;
  mutable prune: int;
  mutable dups : int;
  mutable maxq : int
}

let incr_expand s e =
  s.expand <- s.expand + e

let incr_gen s g =
  s.gen <- s.gen + g

let incr_prune s =
  s.prune <- s.prune + 1

let incr_dups s =
  s.dups <- s.dups + 1

let set_maxq s v =
  s.maxq <- v

let depth_first_search
    (** returns optional node, stats, optimal (does sol satisfy
	optimal_p), complete (was entire space searched) *)
    ?(optimal_p = (Fn.constantly1 false))
    ?(prune_p = (Fn.constantly2 false))
    ?(prev_best = None)
    ?(halt = [Info.Never])
    ?(log = Fn.no_op2)
    copy_state
    better_p
    leaf_p
    num_children
    get_child
    initial
    =

  let info = (Info.make num_children get_child leaf_p better_p optimal_p
		copy_state prune_p log prev_best halt) in

  let optimal = ref (Info.optimal_so_far info) in

  let rec visit n =
    (* visits entire subtree. side-effects on info *)
    if (Info.halt_p info) then raise Halted
    else if (Info.leaf_p info n) then
      (if (Info.optimal_p info n) then
	 (optimal := true;
	  raise Halted))
    else if (Info.prune_p info n) then ()
    else
      (Info.incr_branches info;
       for i = 0 to ((num_children n) - 1) do
	 visit (Info.get_child info n i)
       done)
  in
  let complete = (if !optimal then
		    Info.leaf_or_prune_p info initial
		  else
		    try
		      visit initial;
		      true
		    with Halted | Sys.Break -> false) in
    (Info.curr_best info), (Info.stats info), !optimal, complete


let depth_first_limited root expand goal_p key value bound stats =
  (** depth-first search of all nodes for whom [value] is within
      [bound] (including equality).  returns an optional node of the
      first goal found, the number of nodes expanded and generated,
      and the smallest value seen that is above [bound] *)
  let next_bound = ref infinity in
  let rec visit state prev =
    (** [state]'s key should already be in [prev] *)
    if goal_p state then
      Some state
    else
      let v = value state in
        if v > bound then
          (if v < !next_bound then next_bound := v;
           None)
        else
          let children = expand state in
            incr_expand stats 1;
            incr_gen stats (List.length children);
            iterate prev children
  and iterate prev children =
    match children with
        [] -> None
      | child::rest ->
          let key = key child in
            (* cycle checking *)
            match (if (List.mem key prev) then
                     (incr_dups stats;
		       None)
                   else
                     visit child (key::prev)) with
                None -> iterate prev rest
              | result -> result
  in
  let sol = visit root [(key root)] in
    sol, !next_bound


let wrap_expand expand =
  (** given an expand function that expects to take and return g
      values, returns one that doesn't *)
  (fun s ->
     List.map (fun (c,g) -> c)
       (expand s 0))


let depth_first root expand goal_p key =
  (** limited to depth of max_int *)
  let stats = {expand = 0; gen = 0; dups = 0; prune = 0; maxq = 1} in
  let s, _ = (depth_first_limited root (wrap_expand expand) goal_p key
                      (fun _ -> 0.) infinity stats) in
    s, stats.expand, stats.gen


type 'a node = {
  f : float;
  g : float;
  mutable heap_pos : int;
  state : 'a;
}

let better a b =
  a.f <= b.f

let set_pos n i =
  n.heap_pos <- i


let closed_pos = -1
let initial_pos = -99
  (** must be invalid positions in heap (eg, negative) *)

(** cannot reproduce Korf et all's results for sliding tile puzzles.
    Costs are correct, but expansion / generation counts are off.
    Suitable for a baseline, but should be corrected before publishing any results
    Suggested problems:
      Child ordering
      bad counting practices
**)

(*
let ida_star ?(lim = [Info.Never]) h root expand goal_p key =
  let root = { f = h root;
               g = 0.;
               heap_pos = initial_pos;
               state = root; }
  and expand n =
    List.map (fun (c, g) ->
                { f = g +. (h c);
                  g  = g;
                  heap_pos = initial_pos;
                  state = c; })
      (expand n.state n.g)
  and goal_p n = goal_p n.state
  and stats = {expand = 0; gen = 0; prune = 0; dups = 0; maxq = 1}
  and key n = key n.state
  and value n = n.f
  and incum = (Info.make num_children get_child leaf_p better_p optimal_p
		 copy_state prune_p log prev_best lim) in
  let rec do_pass bound =
    let s, next = (depth_first_limited root expand goal_p key
                           value bound stats) in
      if not (Info.halt_p incum)
      then
	match s with
            None ->
              if next <> bound then
		do_pass next
              else
		None, stats.expand, stats.gen, stats.prune,
	      stats.maxq, stats.dups
          | Some n -> Some (n.state, n.f), stats.expand, stats.gen,
	      stats.prune, stats.maxq, stats.dups
      else None, stats.expand, stats.gen, stats.prune, stats.maxq, stats.dups
  in
    do_pass root.f


let depth_first_id root expand goal_p key h =
  ida_star h root expand goal_p key
*)

let test () =
  depth_first_search
    ~optimal_p:Bounded_depth_test.optimal_p
    Fn.identity
    Bounded_depth_test.better_p
    Bounded_depth_test.leaf_p
    Bounded_depth_test.num_children
    Bounded_depth_test.get_child
    (Bounded_depth_test.make_initial 2 3)

(* EOF *)
