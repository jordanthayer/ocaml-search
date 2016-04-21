(* $Id: csp_instances.ml,v 1.1 2004/01/13 23:45:08 ruml Exp ruml $

   making and storing CSP problems
*)

open Csp


(*************  utilities ************)


let make_domain n =
  Wrutils.map_n Wrutils.identity (n - 1)


let make_domains n m =
  Array.init n (fun _ -> make_domain m)


let satisfiable_p csp =
  let sol, stats, sat, complete =
    Csp_tree_algs.dfs csp Info.Never Info.null_logger
  in
    check (Csp_tree.sol sol) csp sat;
    if not sat then assert complete;
    sat


(***** preassigning values ******)


(* "assignment" means reduce the domain to a singleton *)


let num_singleton d =
  Wrarray.count Wrlist.is_singleton d


let random_non_singleton d =
  Wrlist.random_elt (Wrarray.fold_lefti (fun l i x ->
					   if Wrlist.is_singleton x
					   then l
					   else i::l)
		       [] d)


exception Irrelevant
let no_such_var = -1

let rec forward_reduce d ngs var =
  List.iter (fun pairs ->
	       try
		 let forced = ref no_such_var
		 and bad_val = ref no_such_var in
		   List.iter (fun (var, value) ->
				match d.(var) with
				  [] -> failwith "stumbled into empty domain"
				| [v] when v == value -> ()
				| [_] -> raise Irrelevant
				| _ -> (* unassigned *)
				    if (!forced == no_such_var) then
				      (forced := var;
				       bad_val := value)
				    else
				      raise Irrelevant)
		     pairs;
		   if !forced == no_such_var then
		     (* nogood matched, but don't complain (we're
			matching my old lisp code) *)
		     ()
		   else
		     (* was unit clause *)
		     (match Wrlist.remove_first !bad_val d.(!forced) with
			[] -> (* empty domain, but again, don't complain *)
			  ()
		      | v::[] ->
			  d.(!forced) <- [v];
			  forward_reduce d ngs !forced
		      | l -> d.(!forced) <- l)
	       with Irrelevant -> ())
    ngs.(var)


let assign_values csp percent =
  (** returns a fresh csp like [csp] with with values "assigned" to
    [percent] of [csp]'s variables.  This is done by reducing the domain
    of the variable to a single value.  Forward checking is used in a
    weak way to avoid obviously bad combinations of assignments *)
  let n = num_vars csp in
  let desired_set = Math.round (percent *. (float n))
  and d, ngs = Csp.reduce_domains csp in
  let i = Csp_tree.index_nogoods n ngs in
    while (num_singleton d) < desired_set do
      let var = random_non_singleton d in
      let value = Wrlist.random_elt d.(var) in
	d.(var) <- [value];
	forward_reduce d i var
    done;
    { Csp.domains = d;
      Csp.nogoods = ngs; }


(***** latin squares *****)


let latin_square_nogoods order =
  let ngs = ref [] in
  let var row col =
    (row * order) + col
  and add_all a b =
    for value = 0 to order - 1 do
      Wrutils.push [a, value; b, value] ngs
    done
  in
    for row = 0 to order - 2 do
      for col = 0 to order - 2 do
	let a = var row col in
	  for row1 = row + 1 to order - 1 do
	    let b = var row1 col in
	      add_all a b
	  done;
	  for col1 = col + 1 to order - 1 do
	    let b = var row col1 in
	      add_all a b
	  done
      done
    done;
    !ngs


let make_latin_square order =
  let n = order * order in
    { domains = make_domains n order;
      nogoods = latin_square_nogoods order; }


let make_latin_square_completion order preassigned =
  assign_values (make_latin_square order) preassigned


(*** special latin square I/O ***)


let data_root = "/tilde/ruml/projects/csp/data"


let store_latin_square attrs p =
  Wrio.with_outfile (Rdb.path_for data_root
		       (Rdb.merge_attrs ["type", "instance"] attrs))
    (fun ch ->
       let pf = Verb.pf Verb.always in
	 pf ch "%d\n" (num_vars p);
	 Array.iter (fun dom ->
		       pf ch "%d " (List.length dom);
		       Wrlist.fprint ch string_of_int " " dom;
		       pf ch "\n")
	   p.domains;
	 pf ch "latin square nogoods\n")



let make_some_latin_squares () =
  let orders = [(*11; 13; 15; 17; 19;*)
		21
		(* ; 25; 29; 33*)
	       ]
  and num_at_order = 1000
  and preassigned = 0.3 in
    Random.self_init ();
    List.iter (fun order ->
		 for num = 1 to num_at_order do
		   let p = make_latin_square_completion order preassigned
		   and attrs = ["type", "instance";
				"model", "latin_square";
				"preassigned", string_of_float preassigned;
				"order", string_of_int order;
				"instance_num", string_of_int num] in
		     Wrutils.pr "%d at %d.\n%!" num order;
		     store_latin_square attrs p
		 done)
      orders


(********** basic I/O *************)


let write_csp ch p =
  let pf = Verb.pf Verb.always in
    pf ch "%d\n" (num_vars p);
    Array.iter (fun dom ->
		  pf ch "%d " (List.length dom);
		  Wrlist.fprint ch string_of_int " " dom;
		  pf ch "\n")
      p.domains;
    pf ch "%d\n" (List.length p.nogoods);
    List.iter (fun l ->
		 pf ch "%d " (List.length l);
		 List.iter (fun (var, value) -> pf ch "%d %d " var value) l;
		 pf ch "\n")
      p.nogoods


let store_csp attrs p =
  Wrio.with_outfile (Rdb.path_for data_root
		      (Rdb.merge_attrs ["type", "instance"] attrs))
    (fun ch -> write_csp ch p)


let read_csp ch =
  let get_int () = Wrio.input_int ch in
  let num_vars = get_int () in
  let doms = Array.init num_vars (fun _ ->
				    let size = get_int () in
				      Wrutils.map_ntimes get_int size) in
  let line = input_line ch in
    if line = "latin square nogoods" then
      let order = truncate (sqrt (float num_vars)) in
	assert (num_vars = order * order);
	{ domains = doms;
	  nogoods = latin_square_nogoods order; }
    else
      let num_ngs = int_of_string line in
      let ngs = Wrutils.map_ntimes
		  (fun () ->
		     let num_pairs = get_int () in
		       Wrutils.map_ntimes (fun () ->
					   let var = get_int () in
					   let value = get_int () in
					     var, value)
			 num_pairs)
		  num_ngs in
	{ domains = doms;
	  nogoods = ngs; }


let load_csp path =
  Wrio.with_infile path read_csp


(***** random binary CSPs *****)


let random_pair n =
  (* returns a random pair of numbers, each from 0-(n-1), such that
     the numbers are different *)
  let a = Random.int n in
  let b = Wrutils.eval_until (fun () -> Random.int n)
	    (fun x -> x != a)
  in a, b


let random_binary n m num_pairs num_per =
  (** num_vars num_vals percent_pairs percent_vals *)
  let pairs = Dset.create num_pairs in
    Wrutils.ntimes (fun () ->
		    Wrutils.iter_until (fun () -> random_pair n)
		    (fun (a,b) ->
		       Dset.add_new_p (if a < b then (a,b) else (b, a)) pairs))
      num_pairs;
    let ngs = Dset.create (num_pairs * num_per) in
      Dset.iter (fun (a, b) ->
		   Wrutils.ntimes (fun () ->
				   Wrutils.eval_until (fun () ->
						       [ a, (Random.int m);
							 b, (Random.int m) ])
				   (fun ng -> Dset.add_new_p ng ngs))
		   num_per)
	pairs;
      { domains = make_domains n m;
	nogoods = Dset.to_list ngs; }


let binary_params = [(30, 15, 174, 69);
		     (30, 15, 174, 72);
		     (30, 15, 174, 75);
		     (30, 15, 174, 78);
		     (30, 15, 174, 81);
		     (50, 12, 250, 44);
		     (50, 12, 250, 46);
		     (50, 12, 250, 48);
		     (50, 12, 250, 50);
		     (50, 12, 250, 52);
		     (100, 6, 305, 11);
		     (100, 6, 305, 12);
		     (100, 6, 305, 13)]


let list_binary_params () =
  (** NOT the ones used by Walsh or listed in [binary_params] *)
  Wrlist.mapcan (fun (n, m, p1) ->
		   let n1 = Math.round ((Math.div (n * (n - 1)) 2) *. p1) in
		     List.map (fun p2 ->
				 let n2 = Math.round ((float (m * m)) *. p2) in
				   n, m, n1, n2)
		       [0.306; 0.32; 0.333; 0.347; 0.361])
    [ 30, 15, 0.4;
      50, 12, 0.2;
      100, 6, 0.06; ]


let make_random_binarys n m p1 p2 num =
  for i = 1 to num do
    let attrs = [ "type", "instance";
		  "model", "random_binary";
		  "num_vars", string_of_int n;
		  "num_vals", string_of_int m;
		  "p1", string_of_int p1;
		  "p2", string_of_int p2;
		  "instance_num", string_of_int i; ] in
      (match Rdb.matching_paths data_root attrs with
	 [] ->
	   let p = Wrutils.eval_until
		     (fun () -> random_binary n m p1 p2)
		     satisfiable_p in
	     store_csp attrs p;
	     Wrutils.pr "."
       | path::[] -> Wrutils.pr "skipping %s\n" path
       | _ -> failwith "multiple instances?");
      flush_all ()
  done;
  Wrutils.pr "yup\n%!"


let make_random_binary num =
  (* generates random satisfiable binary CSPs *)
  Random.self_init ();
  List.iter (fun (n,m,p1,p2) -> make_random_binarys n m p1 p2 num)
    binary_params


(* EOF *)
