(** Binary tree where the edge costs are normally distributed.

    @author eaburns
    @since 2010-11-28
*)

open Printf
open Scanf
open Fn

type inst = {
  init_seed : int;
  goal : int list;
  mean : float;
  stdev : float;
}


type node = {
  path : int list;
  seed : int;
  (* seed used to generate children. *)
}


let branch_costs ~mean ~stdev =
  (** [branch_costs ~mean ~stdev] gets the branch costs. *)
  let a = Math.fmax 0. (Wrrandom.log_normal mean stdev) in
  let b = Math.fmax 0. (Wrrandom.log_normal mean stdev) in
    if a < b then a, b else b, a


let random_seed () =
  (** [random_seed ()] makes a random seed. *)
  let max_rand_int = 1073741823 in
    Random.int max_rand_int


(** {1 Instances} ****************************************)


let random_instance ~mean ~stdev ~p depth =
  (** [random_instance ~mean ~stdev ~p depth] generates a random
      instance using the specified normal distribution.  The goal lies
      down a random trajectory at depth [depth].  The goal trajectory
      is biased toward the cheap-cost branch with a probability
      [p]. *)
  let seed = random_seed () in
  let goal = ref [] in
  let generate_path () =
    let next_branch _ = if (Random.float 1.) < p then 0 else 1 in
    let path = Array.init depth next_branch in
      goal := Array.to_list path;
  in
    Wrrandom.with_seed generate_path () seed;
    { init_seed = seed; goal = !goal; mean = mean; stdev = stdev }



let write inst ch =
  (** [write inst ch] writes [inst] to the given channel. *)
  fprintf ch "seed: %d\n" inst.init_seed;
  fprintf ch "mean: %f\n" inst.mean;
  fprintf ch "stdev: %f\n" inst.stdev;
  fprintf ch "goal:";
  List.iter (fprintf ch " %d") inst.goal;
  fprintf ch "\n"


let read ch =
  if (Wrio.read_token ch) <> "seed:" then failwith "Expected seed";
  let seed = Wrio.input_int ch in
    if (Wrio.read_token ch) <> "mean:" then failwith "Expected mean";
    let mean = Wrio.input_float ch in
      if (Wrio.read_token ch) <> "stdev:" then failwith "Expected stdev";
      let stdev = Wrio.input_float ch in
	if (Wrio.read_token ch) <> "goal:" then failwith "Expected goal";
	let goal = Wrio.read_ints ch in
	  { init_seed = seed; mean = mean; stdev = stdev; goal = goal }


let load path =
  Wrio.with_infile path read


let save inst path =
  Wrio.with_outfile path (write inst)


let make_set ?(mean=0.) ?(stdev=1.5) ?(p=0.75) ~depth ~num =
  (** [make_set ?mean ?stdev ?p ~depth ~num] makes a set of instances. *)
  let root_dir = User_paths.instance_root ^ "synthetic_instances" in
  let n = ref 1 and ncreated = ref 0 in
  let base_attrs = [ "model", "normal-tree";
		     "mean", string_of_float mean;
		     "stdev", string_of_float stdev;
		     "p", string_of_float p;
		     "depth", string_of_int depth;
		   ]
  in
    while !ncreated < num do
      let attrs = base_attrs @ [ "num", string_of_int !n ] in
      let path = Rdb.path_for root_dir attrs in
	if not (Sys.file_exists path)
	then begin
	  let inst = random_instance ~mean ~stdev ~p depth in
	    save inst path;
	    incr ncreated;
	end else Verb.pr Verb.always "Skipping %s\n" path;
	incr n;
    done


(** {1 Searching} ****************************************)

let expand inst node g =
  (** [expand inst node] expands a node. *)
  let left_cost = ref nan and right_cost = ref nan in
  let left_seed = ref ~-1 and right_seed = ref ~-1 in
  let kids () =
    let min, max = branch_costs inst.mean inst.stdev in
      left_cost := min;
      right_cost := max;
      left_seed := random_seed ();
      right_seed := random_seed ();
  in
    Wrrandom.with_seed kids () node.seed;
    Verb.pr Verb.debug "left: cost=%g, seed=%d\n" !left_cost !left_seed;
    Verb.pr Verb.debug "right: cost=%g, seed=%d\n" !right_cost !right_seed;
    [ { path = 0 :: node.path; seed = !left_seed }, g +. !left_cost;
      { path = 1 :: node.path; seed = !right_seed }, g +. !right_cost; ]


let update_parent _ _ = ()


let heuristic _ = 0.


let state_type _ = 0


let get_sol_length n = List.length n.path


let key n = n.path


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


let is_goal inst n =
  int_lists_eq inst.goal n.path


let distance inst n =
  if is_goal inst n then 0. else 1.


let default_interface inst limit =
  (** [default_interface inst limit] gets the default search
      interface. *)
  let initial = { path = []; seed = inst.init_seed } in
  let distance = distance inst in
  let hd s = heuristic s, distance s in
    Search_interface.make
      ~h:heuristic
      ~d:distance
      ~t:state_type
      ~hd
      ~domain_expand:(expand inst)
      ~key
      ~key_print:string_of_key
      ~hash
      ~goal_p:(is_goal inst)
      ~halt_on:limit
      ~get_sol_length
      ~equals:eq
      ~p_update:update_parent
      Search_interface.Synthetic
      initial
      (fun _ _ -> false)		(* better_p? *)
      (fun _ -> ())			(* log? *)
