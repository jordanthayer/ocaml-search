(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $

   dynamic programming
*)


let print_row a =
  Array.iter (fun c -> Wrutils.pr "%6d" c) a;
  Wrutils.pr "\n"


let pair_optimal_cost a b =
  (** cost of optimal alignment of two strings *)
  let an = String.length a
  and bn = String.length b in
    (* a runs vertically, b runs horizontally.  note that cell at i,j is
       cost of consuming i chars from a and j chars from b.  we collapse the
       grid into a single row which is re-used.  in fact, we need to cache the
       most recent value so that we don't pre-maturely overwrite the value to
       the `diagonal left' of the current cell. *)
    let row = Array.init (bn+1) (fun i -> i * Msa_instance.gap_cost) in
       (* print_row row; *)
      for a_i = 0 to an-1 do
	(* don't update actual cell until we have used it for diagonal *)
	let new_val = ref (row.(0) + Msa_instance.gap_cost)
	and b_i = ref 0 in
	  while !b_i < bn do
	    (* now computing cell at a_i + 1, b_i + 1 *)
	    let diag = (if a.[a_i] = b.[!b_i] then
			  row.(!b_i)
			else
			  row.(!b_i) + Msa_instance.subst_cost)
	    and left = !new_val + Msa_instance.gap_cost in
	      (* can now update previous cell *)
	      row.(!b_i) <- !new_val;
	      (* update b_i to cell we are working on *)
	      incr b_i;
	      let above = row.(!b_i) + Msa_instance.gap_cost in
		new_val := Math.imin (Math.imin diag left) above
	  done;
	  row.(!b_i) <- !new_val ;
	   (* print_row row; *)
      done;
      row.(bn)


let pair_optimal_cost_slow a b =
  (** uses n^2 space and is written inefficiently, but is likely correct.
    Almost twice the time of the above optimized version. *)
  Wrutils.pr "\n";
  let an = String.length a
  and bn = String.length b in
  let grid = Array.make_matrix (an+1) (bn+1) 0 in
    grid.(0) <- Array.init (bn+1) (fun i -> i * Msa_instance.gap_cost);
    (* print_row grid.(0); *)
    for a_i = 1 to an do
      grid.(a_i).(0) <- a_i * Msa_instance.gap_cost;
      for b_i = 1 to bn do
	let prev_a_i = a_i - 1
	and prev_b_i = b_i - 1 in
	let diag = grid.(prev_a_i).(prev_b_i) +
		   (if a.[prev_a_i] = b.[prev_b_i] then 0
		    else Msa_instance.subst_cost)
	and left = grid.(a_i).(prev_b_i) + Msa_instance.gap_cost
	and above = grid.(prev_a_i).(b_i) + Msa_instance.gap_cost in
	  grid.(a_i).(b_i) <- min diag (min left above);
      done;
      (* print_row grid.(a_i); *)
    done;
    grid.(an).(bn)


let test an bn =
  let i = [| Msa_instance.random_seq an 20;
	     Msa_instance.random_seq bn 20; |] in
  let truth, tt = Wrsys.with_time
		    (fun () -> pair_optimal_cost_slow i.(0) i.(1))
  and fast, tf = Wrsys.with_time
		   (fun () -> pair_optimal_cost i.(0) i.(1)) in
    Wrutils.pr "Got %d in %.3f secs versus %.3f (%.2f).\n"
      fast tf tt (tf /. tt);
    if fast <> truth then
      Wrutils.pr "############ truth was %d ################\n" truth


(* EOF *)
