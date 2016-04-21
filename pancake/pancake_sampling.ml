(** For working with random samples from the pancake domain.

    @author eaburns
    @since 2009-12-05
*)

type state = {
  contents : Pancake.positions;
  g : float;
  h : float;
  t : int;
  successors : state list;
}

let sample ~ncakes f ~ancestors ~num =
  (** [sample ncakes f num] generates a sample of [num] random states
      from a pancakes puzzle size [ncakes] and calls [f] on each.

      The data from the samples assumes the canonical goal. *)
  let rand_state () = let a = Pancake.goal ncakes in Wrarray.permute a; a in
  let lookup_h = Pancake.h_gap in
  let cost = Pancake.cost_function_by_name ncakes "unit" in
  let expand = Pancake.do_expand ncakes cost in
  let t = Pancake.instance_type in
  let rec expand_out ?(depth=0) state g =
    (* expand out to depth=[ancestors]. *)
    if depth = ancestors
    then []
    else begin
      let children = expand state g in
	List.fold_left (fun lst (child, child_g, _) ->
			  let child_h = lookup_h child in
			  let children =
			    expand_out ~depth:(depth + 1) child child_g
			  in { contents = child;
			       g = child_g;
			       h = child_h;
			       t = t child;
			       successors = children } :: lst)
	  []
	  children
    end
  in
    for i = num - 1 downto 0 do
      if Math.divisible i 10_000
      then Verb.pr Verb.optional "%d remaining\n%!" i;
      let state = rand_state () in
      let h = lookup_h state in
	f { contents = state;
	    g = 0.;
	    h = h;
	    t = t state;
	    successors = (expand_out state 0.) }
    done


let print_sample_state outch cols s =
  (** [print_sample_state outch cols s] prints a sample state to the given
      channel. *)
  let rec prefix depth =
    (* get the indent string for a state at depth=[depth]. *)
    if depth = 0 then ""
    else Wrutils.str "    %s" (prefix (depth - 1))
  in
  let rec do_print_contents depth s =
    let p = prefix depth in
      Wrutils.pf outch "%s" p;
      Wrarray.write_ints outch s.contents;
      Wrutils.pf outch "%sg=%f\n" p s.g;
      Wrutils.pf outch "%sh=%f\n" p s.h;
      Wrutils.pf outch "%st=%d\n" p s.t;
      List.iter (fun s' -> do_print_contents (depth + 1) s') s.successors;
      Wrutils.pf outch "%s--------------------\n" p
  in do_print_contents 0 s
