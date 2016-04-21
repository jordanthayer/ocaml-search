(** Generating instances.

    @author eaburns
    @since 2010-03-10
*)

let dock_robot_instances = User_paths.instance_root ^ "dock_robot"

type t = {
  adjacent : float array array;
  (* Adjacency matrix of the distances between each location. *)

  pile_contents : int list array;
  (* The pile stacks.  The first element on each list is the bottom
     stack element for the given pile. *)

  cranes_at_location : int list array;
  (* The list of cranes at each location. *)

  piles_at_location : int list array;
  (* The list of piles at each location. *)

  goal : int array;
  (* The desired location for each container. *)

  nlocations : int;
  (* Locations are numbered 0..nlocations - 1. *)

  ncranes : int;
  (* Cranes are numbered 0..ncranes - 1. *)

  ncontainers : int;
  (* Containers are numbered 0..ncontainers - 1. *)

  npiles : int;
  (* Piles are numbered 0..npiles - 1. *)

  nrobots : int;
  (* Robots are numberef 0..nrobots - 1. *)
}
let pile_location inst p =
  (** [pile_location inst p] gets the location of pile [p].  This is slow
      and should not be used during search. *)
  let loc = ref ~-1
  and l = ref 0 in
    while !loc < 0 && !l < inst.nlocations do
      if List.mem p inst.piles_at_location.(!l) then loc := !l;
      incr l
    done;
    assert (!loc >= 0);
    !loc

(** {6 I/O} ****************************************)

open Printf

let write ch inst =
  let b = Buffer.create 100 in
    Buffer.add_string b (sprintf "nlocations: %d\n" inst.nlocations);
    Buffer.add_string b (sprintf "ncranes: %d\n" inst.ncranes);
    Buffer.add_string b (sprintf "ncontainers: %d\n" inst.ncontainers);
    Buffer.add_string b (sprintf "npiles: %d\n" inst.npiles);
    Buffer.add_string b (sprintf "nrobots: %d\n" inst.nrobots);
    Array.iteri (fun loc adj ->
		   Buffer.add_string b (sprintf "location %d\n" loc);
		   Buffer.add_string b "\tadjacent: ";
		   Array.iter (fun a ->
				 Buffer.add_string b (sprintf "%f " a))
		     adj;
		   Buffer.add_string b "\n";
		   if inst.cranes_at_location.(loc) <> [] then begin
		     Buffer.add_string b "\tcranes: ";
		     List.iter (fun c ->
				  Buffer.add_string b (sprintf "%d " c))
		       inst.cranes_at_location.(loc);
		     Buffer.add_string b "\n";
		   end;
		   if inst.piles_at_location.(loc) <> [] then begin
		     Buffer.add_string b "\tpiles: ";
		     List.iter (fun p ->
				  Buffer.add_string b (sprintf "%d " p))
		       inst.piles_at_location.(loc);
		     Buffer.add_string b "\n";
		   end;)
      inst.adjacent;
    Array.iteri (fun pile contents ->
		   Buffer.add_string b (sprintf "pile %d\n\t" pile);
		   List.iter (fun c ->
				Buffer.add_string b (sprintf "%d " c))
		     contents;
		   Buffer.add_string b "\n";)
      inst.pile_contents;
    Array.iteri (fun container location ->
		   Buffer.add_string b
		     (sprintf "container %d at %d\n" container location))
      inst.goal;
    Printf.fprintf ch "%s\n" (Buffer.contents b)


let save ?(overwrite=false) model inst attrs =
  let attrs = [ "model", model;
		"ncranes", string_of_int inst.ncranes;
		"ncontainers", string_of_int inst.ncontainers;
		"npiles", string_of_int inst.npiles;
		"nrobots", string_of_int inst.nrobots;
		"nlocations", string_of_int inst.nlocations; ] @ attrs
  in
  let path = Rdb.path_for dock_robot_instances attrs in
    if overwrite || not (Sys.file_exists path)
    then begin
      Verb.pr Verb.optional "Saving %s\n" path;
      Wrio.with_outfile path (fun outch -> write outch inst)
    end else Verb.pr Verb.optional "Skipping %s\n" path


let parse_sizes ch =
  let nlocations =
    assert ((Wrio.read_token ch) = "nlocations:");
    Wrio.input_int ch
  and ncranes =
    assert ((Wrio.read_token ch) = "ncranes:");
    Wrio.input_int ch
  and ncontainers =
    assert ((Wrio.read_token ch) = "ncontainers:");
    Wrio.input_int ch
  and npiles =
    assert ((Wrio.read_token ch) = "npiles:");
    Wrio.input_int ch
  and nrobots =
    assert ((Wrio.read_token ch) = "nrobots:");
    Wrio.input_int ch
  in nlocations, ncranes, ncontainers, npiles, nrobots


let read ch =
  let nlocations, ncranes, ncontainers, npiles, nrobots = parse_sizes ch in
  let pile_contents = Array.create npiles []
  and cranes_at_location = Array.create nlocations []
  and piles_at_location = Array.create nlocations []
  and adjacent = Array.create nlocations [||]
  and goal = Array.create ncontainers ~-1 in
  let loc = ref ~-1 in
  let finished = ref false in
    while not !finished do
      match Wrio.read_token ch with
	| "location" -> loc := Wrio.input_int ch
	| "adjacent:" ->
	    assert (!loc >= 0);
	    let adj = Vector.read ch nlocations in
	      assert ((Array.length adj) = nlocations);
	      adjacent.(!loc) <- adj
	| "cranes:" ->
	    assert (!loc >= 0);
	    cranes_at_location.(!loc) <- (Wrio.read_ints ch)
	| "piles:" ->
	    assert (!loc >= 0);
	    piles_at_location.(!loc) <- (Wrio.read_ints ch)
	| "container" ->
	    let c = Wrio.input_int ch in
	    let at = Wrio.read_token ch in
	    let l = Wrio.input_int ch in
	      assert (at = "at");
	      goal.(c) <- l
	| "pile" ->
	    let p = Wrio.input_int ch in
	      pile_contents.(p) <- (Wrio.read_ints ch)
	| _ -> finished := true
    done;
    {
      adjacent = adjacent;
      pile_contents = pile_contents;
      cranes_at_location = cranes_at_location;
      piles_at_location = piles_at_location;
      goal = goal;
      nlocations = nlocations;
      ncranes = ncranes;
      ncontainers = ncontainers;
      npiles = npiles;
      nrobots = nrobots;
    }

let load path = Wrio.with_infile path read

(** {6 Mazes} ****************************************)

let node_num_mapping graph =
  (** [node_num_mapping graph] gets the nodes in the graph numbered as
      they are encountered during a breadth-first traversal. *)
  let o = Queue.create ()
  and num_of_node = Hashtbl.create 100
  and node_of_num = Hashtbl.create 100 in
  let count = ref 0 in
    Queue.push graph.Vacuum_maze.start o;
    while not (Queue.is_empty o) do
      let node = Queue.pop o in
	if not (Hashtbl.mem num_of_node node)
	then begin
	  Hashtbl.add num_of_node node !count;
	  Hashtbl.add node_of_num !count node;
	  incr count;
	  let neighbors = Array.length node.Vacuum_maze.adj in
	    for i = 0 to neighbors - 1 do
	      match node.Vacuum_maze.adj.(i) with
		| Some edge -> Queue.push edge.Vacuum_maze.node o
		| None -> ()
	    done
	end
    done;
    num_of_node, node_of_num


let build_adj graph =
  (** [build_adj graph] builds the adjacency lists. *)
  let num_of_node, node_of_num = node_num_mapping graph in
  let nnodes = Hashtbl.length num_of_node in
  let adj = Array.make_matrix nnodes nnodes infinity in
    for i = 0 to nnodes - 1 do
      let node = Hashtbl.find node_of_num i in
	Verb.pr Verb.debug "%d: (%d, %d)\n"
	  i node.Vacuum_maze.x node.Vacuum_maze.y;
	Array.iter (function
		      | None -> ()
		      | Some edge ->
			  let neigh = edge.Vacuum_maze.node in
			  let cost = edge.Vacuum_maze.cost in
			  let num = Hashtbl.find num_of_node neigh in
			    adj.(num).(i) <- cost;
			    adj.(i).(num) <- cost)
	  node.Vacuum_maze.adj
    done;
    adj


let maze_locations width height cycle_prob =
  (** [maze_locations width height cycle_prob] gets a set of locations
      by building a condensed maze. *)
  let grid = Vacuum_instance.maze_board ~dirts:0 width height cycle_prob in
  let graph = Vacuum_maze.of_instance grid in
    if Verb.level Verb.debug then Vacuum_instance.fprint stdout grid;
    build_adj graph


let build_piles nlocations npiles =
  (** [build_piles nlocations npiles] builds a set of piles and
      assigns them to locations. *)
  let pile_locations = Array.init npiles (fun _ -> Random.int nlocations) in
  let piles_at_location = Array.create nlocations [] in
    for i = 0 to npiles - 1 do
      let loc = pile_locations.(i) in
	piles_at_location.(loc) <- i :: piles_at_location.(loc)
    done;
    pile_locations, piles_at_location


let build_goal pile_locations ncontainers =
  (** [build_goal pile_locations ncontainers] builds a goal state
      where each container is at a specific location (must be a location
      with a pile at it). *)
  let npiles = Array.length pile_locations in
    Array.init ncontainers (fun _ ->
			      let p = Random.int npiles in
				pile_locations.(p))


let permute_list lst =
  (** [permute_list lst] permutes the list. *)
  let a = Array.of_list lst in
    Wrarray.permute a;
    Array.to_list a


let fill_piles npiles ncontainers =
  (** [fill_piles npiles ncontainers] adds some containers to the
      piles. *)
  let container_piles = Array.init ncontainers (fun _ -> Random.int npiles) in
  let pile_contents = Array.create npiles [] in
    for i = 0 to ncontainers - 1 do
      let p = container_piles.(i) in
	pile_contents.(p) <- i :: pile_contents.(p)
    done;
    Array.iteri
      (fun p contaners -> pile_contents.(p) <- permute_list contaners)
      pile_contents;
    pile_contents


let fprintf ch inst =
  Array.iteri
    (fun i adj ->
       Printf.fprintf ch "%d: " i;
       Printf.fprintf ch "\tAdj: ";
       Array.iter (Printf.fprintf ch "%f ") adj;
       Printf.fprintf ch "\n\tCranes: ";
       List.iter (Printf.fprintf ch "%d ") inst.cranes_at_location.(i);
       Printf.fprintf ch "\n\tPiles:\n";
       List.iter (fun p ->
		    let contents = inst.pile_contents.(p) in
		      Printf.fprintf ch "\t\t%d: " p;
		      List.iter (Printf.fprintf ch "%d ") contents;
		      Printf.fprintf ch "\n";)
	 inst.piles_at_location.(i))
    inst.adjacent;
  Printf.fprintf ch "Goal:\n";
  Array.iteri (fun c l -> Printf.fprintf ch "%d @ %d\n" c l) inst.goal


let of_maze ~width ~height cycle_prob ~npiles ~ncontainers =
  (** [of_maze ~width ~height cycle_prob ~npiles ~ncontainers] each
      location with at least one pile will have a crane. *)
  let adj = maze_locations width height cycle_prob in
  let nlocations = Array.length adj in
  let pile_locations, piles_at_location = build_piles nlocations npiles in
  let goal = build_goal pile_locations ncontainers in
  let pile_contents = fill_piles npiles ncontainers in
  let ncranes = ref 0 in
  let cranes_at_location =
    Array.init nlocations (fun loc ->
			     if piles_at_location.(loc) <> []
			     then begin
			       incr ncranes;
			       [!ncranes - 1]
			     end else [])
  in
  let inst = { adjacent = adj;
	       pile_contents = pile_contents;
	       cranes_at_location = cranes_at_location;
	       piles_at_location = piles_at_location;
	       goal = goal;
	       nlocations = nlocations;
	       ncranes = !ncranes;
	       ncontainers = ncontainers;
	       npiles = npiles;
	       nrobots = 1;
	     }
  in
    if Verb.level Verb.debug then fprintf stdout inst;
    inst


let random_mazes ~width ~height cycle_prob ~npiles ~ncontainers ~num =
  for n = 1 to num do
    let inst = of_maze ~width ~height cycle_prob ~npiles ~ncontainers in
      save "maze" inst ["width", string_of_int width;
			"height", string_of_int height;
			"cycle_prob", string_of_float cycle_prob;
			"num", string_of_int n]
  done

(** {Unit square} ****************************************)


let place_locations nlocs =
  let xs = Array.init nlocs (fun _ -> Random.float 1.) in
  let ys = Array.init nlocs (fun _ -> Random.float 1.) in
    Wrarray.init_matrix nlocs nlocs
      (fun i j ->
	 let xi = xs.(i) and yi = ys.(i) in
	 let xj = xs.(j) and yj = ys.(j) in
	   sqrt ((xi -. xj) ** 2. +. (yi -. yj) ** 2.))


let random_usquare ~nlocs ~piles_per_loc ~cranes_per_loc ~ncontainers =
  let next r () = r := !r + 1 ; !r - 1 in
  let pile = ref 0 and crane = ref 0 and container = ref 0 in
  let locs = Array.init nlocs Fn.identity in
  let adj = place_locations nlocs in
  let piles_at_location =
    Array.map (fun _ -> Wrutils.map_ntimes (next pile) piles_per_loc) locs
  in
  let cranes_at_location =
    Array.map (fun _ -> Wrutils.map_ntimes (next crane) cranes_per_loc) locs
  in
  let pile_contents =
    Array.init !pile
      (fun i ->
	 let left = ncontainers - !container in
	 let num = if i < !pile - 1 then Random.int left else left in
	   Wrutils.map_ntimes (next container) num)
  in
  let goal = Array.init !container (fun _ -> Random.int nlocs) in
    {
      adjacent = adj;
      pile_contents = pile_contents;
      cranes_at_location = cranes_at_location;
      piles_at_location = piles_at_location;
      goal = goal;
      nlocations = nlocs;
      ncranes = !crane;
      ncontainers = !container;
      npiles = !pile;
      nrobots = 1;
    }


let random_usquares ~nlocs ~piles_per_loc ~cranes_per_loc ~ncontainers ~num =
  for n = 1 to num do
    let inst =
      random_usquare ~nlocs ~piles_per_loc ~cranes_per_loc ~ncontainers
    in
      save "usquare" inst
	[ "num", string_of_int n]
  done


(* EOF *)
