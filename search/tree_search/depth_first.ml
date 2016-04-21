(** Depth first search
    Jordan Thayer July 2009 *)


let child_order better_p =
  (fun a b -> if better_p a b
   then 1
   else (if better_p b a
	 then -1
	 else 0))


let no_dups sface better_p =
  let i = sface.Search_interface.info in
  let feasible_p n =
    (match i.Limit.incumbent with
	 Limit.Incumbent(float,m) -> better_p n m
       | _ -> true) in
  let rec expand_best n =
    if not (Limit.halt_p i)
    then
      (Limit.incr_exp i;
       if sface.Search_interface.goal_p n
       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
       else (let children = sface.Search_interface.node_expand n in
	       List.iter (fun c ->
			    Limit.incr_gen i;
			    if feasible_p c
			    then expand_best c)
		 (List.sort (child_order better_p) children))) in
    expand_best sface.Search_interface.initial;
    Limit.results5 i

(* Depth first search with classic cycle detection *)
let dups_cycle sface better_p =
  failwith "Not yet implemented - Dfs BDD, classic cycle detection"


(* Depth first search branch and bound using a hashtbl to detect
   cycles as the search goes on.  Will explore a child which is a duplicate
   if the g value is better. *)
let dups_hash ?(continue = true) ?ordered_p sface better_p =
  let i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and co = child_order (match ordered_p with None -> better_p
			  | Some pred -> pred)
  and solved = ref false
  and openlist = Stack.create() in
  let feasible_p n =
    try
      let best = Htable.find closed (sface.Search_interface.key n) in
	Limit.incr_dups i;
	if better_p n best && continue
	then (Htable.replace closed (sface.Search_interface.key n) n;
	      true)
	else false
    with Not_found ->
      Htable.replace closed (sface.Search_interface.key n) n;
      (match i.Limit.incumbent with
	   Limit.Incumbent(float,m) -> better_p n m
	 | _ -> true) in

  let rec expand_best () =
    let n = Stack.pop openlist in
    if not (Limit.halt_p i)
    then
      (Limit.incr_exp i;
       if sface.Search_interface.goal_p n
       then (solved := true;
	     Verb.pe Verb.always "Solved!\n";
	     Limit.new_incumbent i (Limit.Incumbent (0.,n)))
       else (let children = List.sort co
	       (sface.Search_interface.node_expand n)  in
	       List.iter (fun c ->
			    Limit.incr_gen i;
			    if feasible_p c
			    then Stack.push c openlist
			    else Limit.incr_prune i) children);
       if (not !solved) || continue then expand_best ()) in

    Stack.push sface.Search_interface.initial openlist;
    expand_best ();
    Limit.results6 i



(* Depth first search branch and bound using a hashtbl to detect
   cycles as the search goes on.  Will not explore a child which is a duplicate
   even if the g value is better. *)
let dups_hash_dd sface better_p =
  let i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and co = child_order better_p in
  let feasible_p n =
    try
      ignore (Htable.find closed (sface.Search_interface.key n));
	Limit.incr_dups i;
	false
    with Not_found ->
      Htable.replace closed (sface.Search_interface.key n) n;
      (match i.Limit.incumbent with
	   Limit.Incumbent(float,m) -> better_p m n
	 | _ -> true) in
  let rec expand_best n =
    if not (Limit.halt_p i)
    then
      (Limit.incr_exp i;
       if sface.Search_interface.goal_p n
       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
       else (let children = sface.Search_interface.node_expand n in
	     let children = List.sort co children in
	       List.iter (fun c ->
			    Limit.incr_gen i;
			    if feasible_p c
			    then expand_best c
			    else Limit.incr_prune i)
	    (List.sort (child_order better_p) children))) in
    expand_best sface.Search_interface.initial;
    Limit.results6 i

(* EOF *)
