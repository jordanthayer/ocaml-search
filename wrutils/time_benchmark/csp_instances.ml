(* $Id: csp_instances.ml,v 1.1 2004/01/13 23:45:08 ruml Exp ruml $

   making and storing CSP problems
*)
		   
open Csp


(*************** I/O **************)

    
let data_root = "/tilde/ruml/projects/csp/data"


let write_csp ch p =
  let pf = Printf.fprintf in
    pf ch "%d\n" (num_vars p);
    Array.iter (fun dom ->
		  pf ch "%d " (List.length dom);
		  Wrlist.fprint_list ch string_of_int " " dom;
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
				      Utils.map_ntimes get_int size) in
  let num_ngs = get_int () in
  let ngs = Utils.map_ntimes (fun () ->
				let num_pairs = get_int () in
				  Utils.map_ntimes (fun () ->
						      let var = get_int () in
						      let value = get_int () in
							var, value)
				    num_pairs)
	      num_ngs in
    { domains = doms;
      nogoods = ngs; }


let load_csp path =
  Wrio.with_infile path read_csp
  

(*************  utilities ************)


let make_domain n =
  Utils.map_n Utils.identity (n - 1)


let make_domains n m =
  Array.init n (fun _ -> make_domain m)


let satisfiable_p csp =
  let sol, stats, sat, complete =
    Csp_tree_algs.dfs csp Info.Never Info.null_logger
  in
    check (Csp_tree.sol sol) csp sat; 
    if not sat then assert complete;
    sat

      
(***** random binary CSPs *****)


let random_pair n =
  (* returns a random pair of numbers, each from 0-(n-1), such that
     the numbers are different *)
  let a = Random.int n in
  let b = Utils.eval_until (fun () -> Random.int n)
	    (fun x -> x != a)
  in a, b
      
      
let random_binary n m num_pairs num_per =
  (** num_vars num_vals percent_pairs percent_vals *)
  let pairs = Dset.create num_pairs in
    Utils.ntimes (fun () ->
		    Utils.iter_until (fun () -> random_pair n)
		    (fun (a,b) ->
		       Dset.add_new_p (if a < b then (a,b) else (b, a)) pairs))
      num_pairs;
    let ngs = Dset.create (num_pairs * num_per) in
      Dset.iter (fun (a, b) ->
		   Utils.ntimes (fun () ->
				   Utils.eval_until (fun () ->
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
  (** NOT the ones used by Walsh *)
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
		  "num_vars", string_of_int n;
		  "num_vals", string_of_int m;
		  "p1", string_of_int p1;
		  "p2", string_of_int p2;
		  "instance_num", string_of_int i; ] in
      (match Rdb.matching_paths data_root attrs with
	 [] ->
	   let p = Utils.eval_until
		     (fun () -> random_binary n m p1 p2)
		     satisfiable_p in
	     store_csp attrs p;
	     Utils.pr "."
       | path::[] -> Utils.pr "skipping %s\n" path
       | _ -> failwith "multiple instances?");
      flush_all ()
  done;
  Utils.pr "yup\n%!"
    

let make_random_binary num =
  (* generates random satisfiable binary CSPs *)
  Random.self_init ();
  List.iter (fun (n,m,p1,p2) -> make_random_binarys n m p1 p2 num)
    binary_params

(*
let num_pairs p a b =
  p

  
let random_binary_stats n =
  List.iter (fun path ->
	       let p = load_csp path in
		 foo)
    (Rdb.matching_paths data_root [ "type", "instance";
				    "num_vars", string_of_int n; ])
*)

(***** latin squares *****)


let latin_square_generator params =
  failwith "not implemented"

      
(* EOF *)
