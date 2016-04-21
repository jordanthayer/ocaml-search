(** A decision tree on the assigned values to the SAS+ variables.

    The path through the tree represents different assignments to the
    variables.  Each node in the tree is associated with a variable.
    Each node has a list of the operators that are applicable due
    since all of their conditions are satisfied by the path and an
    array of children.  Child 0 represents conditions that do not rely
    on this variable an the remaining children each are associated
    with a value of the given variable (namely the index is the value
    + 1).

    @author eaburns
    @since 2010-07-14
*)

type 'a node = Leaf of 'a list | Node of int * 'a node array * 'a list


let next_variable ds =
  (** [next_variable ds] gets the next variable to branch on based on
      the variable that appears in the most conditions. *)
  (* TODO: This code should be made better/faster. *)
  let counts = Hashtbl.create 100 in
  let max_vr = ref (~-1) and max_count = ref (~-1) in
    List.iter
      (fun (conds, _) ->
	 List.iter (fun (vr, _) ->
		      let count = (try
				     let _, count = Hashtbl.find counts vr in
				       incr count;
				       !count
				   with Not_found ->
				     Hashtbl.add counts vr (vr, ref 1);
				     1)
		      in
			if count > !max_count
			then begin
			  max_vr := vr;
			  max_count := count
			end)
	   conds)
      ds;
    if !max_count = 0 then ~-1 else !max_vr


let rec remove_cond ?(accum=[]) variable = function
    (** [remove_cond ?accum variable conds] removes the variable from
	the list of conditions resulting in the variable and the
	remaining conditions or raises Not_found. *)
  | [] -> raise Not_found
  | (vr, _) as hd :: tl when vr = variable -> hd, accum @ tl
  | hd :: tl -> remove_cond ~accum:(hd :: accum) variable tl


let build vdomains ds =
  (** [build vdomains ds] builds a condition tree for the conditional
      data.  [vdomains] is an array of ints declaring the size of the
      domain of each variable.  [ds] is a list of the data in the form
      (conds, data). *)
  let rec do_build ds =
    let vr = next_variable ds in
      if vr < 0 || ds = []
      then Leaf (List.map snd ds)
      else begin
	let domain_size = vdomains.(vr) in
	let ds' = Array.create (domain_size + 1) [] in
	let data = ref [] in
	  List.iter (fun ((conds, d) as cur) ->
		       if conds = []
		       then data := d :: !data
		       else begin
			 try
			   let (_, vl), conds' = remove_cond vr conds in
			     ds'.(vl + 1) <- (conds', d) :: ds'.(vl + 1)
			 with Not_found -> ds'.(0) <- cur :: ds'.(0)
		       end)
	    ds;
	  Node (vr, Array.map do_build ds', !data)
      end
  in do_build ds



let iter f root =
  (** [iter f root] iterates [f] over each path/variable combo. *)
  let rec do_iter path f = function
    | Leaf data -> List.iter (f path) data
    | Node (variable, children, data) ->
	List.iter (f path) data;
	Array.iteri (fun i ch ->
		       let path' =
			 if i > 0 then (variable, i - 1) :: path else path
		       in
			 do_iter path' f ch)
	  children
  in do_iter [] f root


let rec for_all_applicable ?(depth=0) vars f = function
    (** [for_all_applicable ?depth vars f root] iterates [f] over each datum
	whose conditions are met by the values of the variables. *)
  | Leaf ds ->
      List.iter f ds;
  | Node (vr, children, ds) ->
      List.iter f ds;
      for_all_applicable ~depth:(depth + 1) vars f children.(vars.(vr) + 1);
      for_all_applicable ~depth:(depth + 1) vars f children.(0)
