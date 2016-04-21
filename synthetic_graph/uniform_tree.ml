(** A uniform tree.

    @author eaburns
    @since 2010-08-01
*)

type t = {
  sol_path : int array;
  (* The solution path given by the branch to take at each depth up to
     the solution depth. *)

  costs : float array;
  (* The cost of the edge between the parent and each child. *)
}

type state = int list
    (* Branches taken so far *)

(** {1 Instances} ****************************************)


let write ch t =
  (** [write ch t] writes the instance to the given channel. *)
  let sol = t.sol_path in
  let costs = t.costs in
    Printf.fprintf ch "%d %d\n" (Array.length costs) (Array.length sol);
    for i = 0 to (Array.length costs)  - 1 do
      if i <> 0 then Printf.fprintf ch " ";
      Printf.fprintf ch "%f" costs.(i);
    done;
    Printf.fprintf ch "\n";
    for i = 0 to (Array.length sol)  - 1 do
      if i <> 0 then Printf.fprintf ch " ";
      Printf.fprintf ch "%d" sol.(i);
    done;
    Printf.fprintf ch "\n"


let read ch =
  (** [read ch] reads the instance from the channel. *)
  let branching, sol_depth = Scanf.fscanf ch " %d %d" (fun b s -> b, s) in
  let costs =
    Array.init branching (fun _ -> Scanf.fscanf ch " %f" Fn.identity) in
  let sol =
    Array.init sol_depth (fun _ -> Scanf.fscanf ch " %d" Fn.identity)
  in
    Verb.pr Verb.toplvl "branching=%d, solution depth=%d\n%!"
      branching sol_depth;
    { costs = costs; sol_path = sol; }


let load path =
  (** [load path] loads an instance from the path. *)
  let ich = open_in path in
  let t = read ich in
    close_in ich;
    t


let save path t =
  (** [save path t] saves the instance to the given path. *)
  Verb.pr Verb.toplvl "Saving %s\n" path;
  Wrio.with_outfile path (fun och -> write och t)


let make_random costs ?max_sol_br min_depth max_depth =
  (** [make_random costs ?max_sol_br min_depth max_depth] makes a
      random instance.  If [max_sol_br] is given then it is the
      maximum child number under which the solution will be, otherwise
      it is just taken to be under any child.  *)
  if max_depth < 1 then invalid_arg "Must have a positive max depth";
  if min_depth < 1 then invalid_arg "Must have a positive min depth";
  if min_depth > max_depth
  then invalid_arg "Min depth must be less than max depth";
  let br = Array.length costs in
  let d =
    if min_depth = max_depth
    then min_depth
    else (Random.int (max_depth - min_depth)) + min_depth in
  let br_max = match max_sol_br with
    | Some b -> b
    | None -> br
  in
    { costs = costs;
      sol_path = Array.init d (fun _ -> Random.int (br_max + 1)); }


let string_of_costs costs =
  (** [string_of_costs costs] gets a string representation of costs. *)
  Wrarray.fold_lefti (fun s i c ->
			if i = 0
			then Printf.sprintf "%s%g" s c
			else Printf.sprintf "%s,%g" s c)
    "" costs


let make_set costs ?max_sol_br ~min_depth ~max_depth ~num =
  (** [make_set costs ?max_sol_br ~min_depth ~max_depth ~num] makes a
      set of instances. *)
  let root_dir = User_paths.instance_root ^ "synthetic_instances" in
  let n = ref 1 and ncreated = ref 0 in
  let costs_str = string_of_costs costs in
  let base_attrs = [ "model", "uniform-tree";
		     "costs", costs_str;
		     "max-sol-branching",
		     (match max_sol_br with
			| None ->
			    string_of_int (Array.length costs)
			| Some br -> string_of_int br);
		     "min-depth", (string_of_int min_depth);
		     "max-depth", (string_of_int max_depth);
		   ]
  in

    while !ncreated < num do
      let attrs = base_attrs @ [ "num", string_of_int !n ] in
      let path = Rdb.path_for root_dir attrs in
	if not (Sys.file_exists path)
	then begin
	  let inst = make_random costs ?max_sol_br min_depth max_depth in
	    save path inst;
	    incr ncreated;
	end else Verb.pr Verb.always "Skipping %s\n" path;
	incr n;
    done


(** {1 Searching} ****************************************)


let update_parent _ _ = ()


let heuristic _ = 0.


let state_type _ = 0


let get_sol_length s = List.length s


let key = Fn.identity


let hash = Hashtbl.hash

let rec int_lists_eq a b =
  match a, b with
    | ahd :: atl, bhd :: btl when (ahd:int) = bhd ->
	int_lists_eq atl btl
    | [], [] -> true
    | _ -> false


let eq a b = int_lists_eq a b


let string_of_key key =
  (** [string_of_key key] gets the string representation of the key. *)
  let buf = Buffer.create 100 in
  let first = ref true in
    List.iter (fun i ->
		 let s = (if !first
			  then begin
			    first := false;
			    Printf.sprintf "%d" i
			  end else Printf.sprintf " %d" i)
		 in Buffer.add_string buf s)
      key;
    Buffer.contents buf


let make_expand t =
  (** [make_expand t] creates the expand function. *)
  let costs = t.costs in
  let br = Array.length costs in
    (fun state g ->
       let kids = ref [] in
	 for i = 0 to br - 1 do
	   kids := (i :: state, g +. costs.(i)) :: !kids
	 done;
	 !kids)


let make_is_goal t =
  (** [make_is_goal t] makes a function for testing if the given state
      is the goal state. *)
  let rec check_path sol_depth sol i = function
    | _ when i > sol_depth -> false
    | [] when i < sol_depth -> false
    | [] -> true
    | br :: brs when (br:int) <> sol.(i) -> false
    | br :: brs -> check_path sol_depth sol (i + 1) brs
  in
  let sol = t.sol_path in
  let sol_depth = (Array.length sol) - 1 in
    check_path sol_depth sol 0



let make_distance t s =
  (** [make_distance t s] makes a distance estimator for [t] that
      gives the value 1. for all non-goal nodes. *)
  let is_goal = make_is_goal t in
    if is_goal s then 0. else 1.


let default_interface t limit =
  (** [default_interface t limit] gets the default search
      interface. *)
  let initial = [] in
  let distance = make_distance t in
  let hd s = heuristic s, distance s in
    Search_interface.make
      ~h:heuristic
      ~d:distance
      ~t:state_type
      ~hd
      ~domain_expand:(make_expand t)
      ~key
      ~key_print:string_of_key
      ~hash
      ~goal_p:(make_is_goal t)
      ~halt_on:limit
      ~get_sol_length
      ~equals:eq
      ~p_update:update_parent
      Search_interface.Synthetic
      initial
      (fun _ _ -> false)		(* better_p? *)
      (fun _ -> ())			(* log? *)
