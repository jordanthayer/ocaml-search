(** Simple searches (for testing).

    @author eaburns
    @since 2009-07-29
*)

let breadth_first is_goal expand init =
  let o = Queue.create () in
  let goal = ref None in
  let expansions = ref 0 in
    Queue.add (init, []) o;
    while !goal = None && not (Queue.is_empty o) do
      let n, path = Queue.take o in
	incr expansions;
	let chlds = expand n in
	  if is_goal n
	  then goal := Some path
	  else List.iter (fun (c, op) -> Queue.add (c, op :: path) o) chlds;
    done;
    !goal, !expansions


type ('a, 'b) astar_node = {
  state : 'a;
  path : 'b list;
  h : float;
  g : float;
  f : float;
}


let wrap_state h parent op s =
  let g = parent.g +. op.Sas.o_cost in
  let h = h s in
    {
      state = s;
      path = op :: parent.path;
      h = h;
      g = g;
      f = g +. h;
    }


let is_better a b =
  if a.f = b.f then a.g > b.g else a.f < b.f


let astar is_goal expand h init =
  let init_node = { state = init;
		    path = [];
		    g = 0.;
		    h = h init;
		    f = h init;
		  } in
  let o = Dpq.create_with is_better init_node in
  let closed = Htable.create Hashtbl.hash (=) 100 in
  let goal = ref None in
  let expansions = ref 0 in
  let generations = ref 0 in
    Printf.printf "h_init=%f\n%!" init_node.h;
    Dpq.insert o init_node;
    while !goal = None && not (Dpq.empty_p o) do
      let n = Dpq.extract_first o in
	(*
	  Printf.printf "h=%f\n%!" n.h;
	*)
	if not (Htable.mem closed n)
	then begin
	  Htable.add closed n n.g;
	  incr expansions;
	  let chlds = expand n.state in
	    if is_goal n.state
	    then goal := Some n.path
	    else
	      List.iter (fun (c, op) ->
			   incr generations;
			   Dpq.insert o (wrap_state h n op c))
		chlds;
	end
    done;
    !goal, !expansions, !generations
