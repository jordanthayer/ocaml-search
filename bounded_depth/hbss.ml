(* $Id: blfs.ml,v 1.2 2004/01/11 01:24:20 ruml Exp ruml $

   stochastic tree probing
*)


let max_prob_exploit run_len run_prob =
  (** probability with which we should do something if we want the
    probability of doing it [run_len] times in a row to be [run_prob] *)
  run_prob ** (1. /. (float run_len))


let log_bias i = 1. /. (log (float (i + 2)))
and lin_bias i = 1. /. (float (i+ 1))
and pol_bias n i = 1. /. (float (i+1))**n
and exp_bias i = 1. /. (exp (float i))
and ran_bias = (fun _ -> Random.float 1.)

let biased_probing
    (** returns optional sol, stats, optimal *)
    max_depth
    ?(max_prob = 0.05)
    ?(optimal_p = (Fn.constantly1 false))
    ?(prune_p = (Fn.constantly2 false))
    ?(prev_best = None)
    ?(halt = [])
    ?(log = Fn.no_op2)
    ?(bias = log_bias)
    copy_state
    better_p
    leaf_p
    num_children
    get_child
    initial =
  let info = Info.make num_children get_child leaf_p better_p optimal_p
    copy_state prune_p log prev_best halt in
  let rec visit n =
    (** returns true iff reached halt condition *)
    if (Info.leaf_p info n) then
      Info.optimal_p info n
    else if (Info.prune_p info n) then
      false
    else if (Info.halt_p info) then
      true
    else
      (Info.incr_branches info;
       (* here is where you could do the prob thang*)
       let tot_weight = ref 0. in
	 (* construct the weights based on the bias function *)
       let weights = (Wrlist.mapi (fun index ele ->
				    let wt = bias index in
				      tot_weight := wt +. !tot_weight;
				      wt, index)
			(Wrlist.range ~min:0 ((num_children n) - 1))) in
	 (* normalize the weights *)
       let weights = List.map (fun (w,i) -> w /. !tot_weight, i) weights
       and rand = Random.float 1. in
	 List.iter (fun i -> assert ((fst i) <= 1.)) weights;
       let (_,selected) = List.fold_left (fun (awt,aind) (wt,index) ->
					    if awt < rand
					    then (wt +. awt, index)
					    else (1., aind)) (0.,0) weights in
	 if (weights = [])
	 then false
	 else visit (Info.get_child info n selected))
       in
	while not (visit initial) do
	  ()
	done;
	(Info.curr_best info),
    (Info.stats info),
    (Info.optimal_so_far info), false


(* EOF *)
