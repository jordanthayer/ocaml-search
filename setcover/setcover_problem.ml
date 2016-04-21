(* $Id: problem.ml,v 1.2 2004/08/17 20:02:10 ruml Exp $

   instances of weighted set cover
*)


type subset = float * int list

type t = {
  num_objs : int;
  subsets : subset array;
}

let instance_root = User_paths.instance_root ^ "setcover/"

let num_subsets p =
  Array.length p.subsets


let max_depth p = Array.length p.subsets
  (** [max_depth p] gets an upper bound on the maximum problem
      depth. *)

(***** I/O *****)


let read ch =
  let num_objs = Wrio.input_int ch in
  let num_subsets = Wrio.input_int ch in
  let subsets = Array.init num_subsets
    (fun _ ->
       let v = Wrio.input_float ch in
       let elts = Wrstr.parse_ints (input_line ch) in
	 v, elts) in
    { num_objs = num_objs;
      subsets = subsets; }


let output ch p =
  (** prints many vals as floats, not ints! *)
  Printf.fprintf ch "%d\n%d\n" p.num_objs (Array.length p.subsets);
  Array.iter (fun (v, elts) ->
		Wrutils.pf ch "%f " v;
		Wrlist.fprint ch string_of_int " " elts;
		Wrutils.newline ch)
    p.subsets


(*********** some utilities ***********)


let subset_eq (c1,s1) (c2,s2) =
  (c1 = c2) && (Wrlist.set_equal s1 s2)


let subset_better (c1,s1) (c2,s2) =
  (** more or equal stuff for less or equal cost *)
  (Wrlist.is_subset s2 s1) && (c1 <= c2)


let undominated p =
  let subsets = ref [] in
    Array.iteri (fun i s ->
		   if not (List.exists (fun i -> subset_eq s p.subsets.(i))
			     !subsets) then
		     Wrutils.push i subsets)
      p.subsets;
    List.filter (fun i ->
		   not (List.exists (fun j ->
				       (j != i) && (subset_better
						      p.subsets.(j)
						      p.subsets.(i)))
			  !subsets))
      !subsets


let remove_dominated p =
  let subsets = Array.of_list (List.map (fun i -> p.subsets.(i))
				 (undominated p)) in
    { p with subsets = subsets }


let uncovered num_objs subsets =
  (** returns list of objs not in any subset *)
  let coverable = Array.make num_objs false in
    List.iter (fun (_,elts) ->
		 List.iter (fun i ->
			      coverable.(i) <- true)
		 elts)
      subsets;
    Wrutils.map_n_opt (fun i ->
		       if coverable.(i) then None else Some i)
      (num_objs - 1)


let ensure_feasible p =
  Array.iter (fun (c,s) ->
		if not (Math.finite_p c) then
		  failwith "bad subset cost";
		List.iter (fun x ->
			     if (x < 0) || (x >= p.num_objs) then
			       failwith (Wrutils.str "%d invalid (num objs %d)"
					   x p.num_objs))
		  s;
		if Wrlist.duplicates_p s then
		  failwith "subset contains duplicates!")
    p.subsets;
  match uncovered p.num_objs (Array.to_list p.subsets) with
    [] -> ()
  | _ -> failwith "some elements are uncoverable!"


(********** constructing random instances **********)

let make_uniform_subset num_objs max_proportion min_value max_value =
  (** [make_uniform_subset num_objs max_proportion min_value
      max_value] makes a subset of elements which are choosen
      uniformly from a superset with [num_objs] objects.  The size of
      the set is uniformly choosen from [1, num_objs
      *. max_proportion).  The value is uniformly choosen in the range
      [min_value, max_value). *)
  let subset_size =
    truncate (Wrrandom.float_in_range 1. ((float num_objs) *. max_proportion))
  and weight = (Random.float (max_value -. min_value)) +. min_value in
  let elts =
    Wrutils.map_ntimes (fun () -> Random.int num_objs) subset_size
  in weight, (Wrlist.remove_duplicates elts)


(** Makes a random instance.

    @param num_objs is the number of objects in the set.

    @param num_subsets is the number of subsets.


    @param max_value is the maximum value of a subset.

    @param max_proportion is the proportion of the number of elements
    in the set to use as an upper-bound for subset size.  Subsets are
    between 1 and [max_proportion *. num_objs] elements. *)
let random_instance num_objs num_subsets min_value max_value max_proportion =
  let make_subset () =
    make_uniform_subset num_objs max_proportion min_value max_value
  in
  let most = Wrutils.map_ntimes make_subset (num_subsets - 1) in
  let extra = (match uncovered num_objs most with
		   [] -> make_subset ()
		 | l -> (Random.float max_value), l) in
    { num_objs = num_objs;
      subsets = Array.of_list (extra :: most); }


let make_uniform num_objs num_subsets max_proportion
    min_value max_value count =
  let attrs =
    [
      "model", "uniform";
      "objects", string_of_int num_objs;
      "subsets", string_of_int num_subsets;
      "min value", string_of_float min_value;
      "max value", string_of_float max_value;
      "max proportion", string_of_float max_proportion;
    ]
  in
    for c = 1 to count do
      let inst_attrs = attrs @ [ "num", string_of_int c ] in
      let path = Rdb.path_for instance_root inst_attrs in
	if not (Sys.file_exists path)
	then begin
	  Wrutils.pr "Saving %s\n%!" path;
	  Wrio.with_outfile path
	    (fun ch ->
	       output ch (random_instance num_objs num_subsets
			    min_value max_value max_proportion))
	end else Wrutils.pr "Skipping %s\n%!" path;
    done


(********** solutions *****************)


type solution = int list


let default_solution p =
  (** useful for returning when no leaf was reached *)
  Wrutils.map_n Fn.identity ((num_subsets p) - 1)


let check_sol p s =
  (** returns cost or raises failure *)
  match uncovered p.num_objs (List.map (fun i -> p.subsets.(i)) s) with
    [] -> Wrlist.sum_floats (fun i -> fst p.subsets.(i)) s
  | l ->
      Wrutils.pr "Subsets: ";
      Wrlist.fprint stdout string_of_int " " s;
      Wrutils.pr "\nUncovered: ";
      Wrlist.fprint stdout string_of_int " " l;
      Wrutils.newline stdout;
      flush_all ();
      failwith "solution leaves element(s) uncovered!"


let print_sol ch s =
  Wrlist.fprint ch string_of_int " " s


(* EOF *)
