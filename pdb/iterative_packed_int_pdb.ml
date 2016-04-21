(**

    @author jtd7
    @since 2010-09-14

   For when using packed ints isn't enough, and you have to take an itterative
   approach to generating the pdb to avoid using an open list.
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  g : int;          (* Cost of reaching a node *)
}

let wrap f = (fun n -> f n.data)
let ordered_p a b = a.g <= b.g


let make_expand expand =
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       g = truncate g;}) (expand n.data (float n.g)))


let dups ?(first_max = 1) size expand key initial equals hash =
  let key = wrap key
  and expand = make_expand expand in

  let rec consider_max max_guess =
    let open_list = Stack.create ()
    and pinfo = Packed_ints.info max_guess in
    let pints = Packed_ints.make_create pinfo size in
    let get = Packed_ints.make_get pinfo pints
    and set = Packed_ints.make_set pinfo pints
    and set_val = Array.create size false in

    let consider_child n =
      if n.g <= max_guess
      then (let kv = key n in
	    let oldg = get kv in
	      if oldg > n.g || (not set_val.(kv))
	      then (set kv n.g;
		    set_val.(kv) <- true;
		    true)
	      else false)
      else false in

    let rec expand_best () =
      if not (Stack.is_empty open_list)
      then (match Stack.pop open_list with
	      | [] -> expand_best ()
	      | n::tl ->
		  (Stack.push tl open_list;
		   let children = expand n in
		   let children =  List.filter consider_child children in
		     (Stack.push children open_list;
		      expand_best ()))) in

      Stack.push [{ data = initial; g = 0}] open_list;
      Verb.pe Verb.always "Current guess at max %i\n%!" max_guess;
      expand_best ();
      Verb.pe Verb.always "Exhausted that tree\n%!";
      let finished = ref true in
	(try
	   for i = 0 to (size - 1) do
	     if not set_val.(i) then (finished := false; assert false)
	   done
	 with _ -> ());
      if !finished
      then (Verb.pe Verb.always "Done\n%!";
	    pints, max_guess)
      else consider_max (max_guess + 1)
  in
    Verb.pe Verb.always "Max Size %i\n%!" size;
    consider_max first_max

(* EOF *)
