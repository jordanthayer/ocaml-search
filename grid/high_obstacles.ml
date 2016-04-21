(** For generating instances with many obstacles (but not mazes) *)

open Grid

type cell_types =
  | Verboten
  | Free
  | Blocked

let make_empty_instance ?(i = -1) cost moves x y =
  { blocked = Array.create_matrix x y false;
    costs = cost;
    moves = moves;
    goal = [(x-1), 0];
    start = 0,0;
    instance = i; }


let make_rand_h seed =
  Random.init seed;
  (fun n -> Random.float 1.)


let df_iface seed w =
  Search_interface.make
    ~h:(make_rand_h seed)
    ~domain_expand:(make_expand w)
    ~key:key
    ~key_print:key_to_string
    ~equals:equals
    ~goal_p:(make_goal_p w)
    ~halt_on:[]
    ~get_sol_length:sol_length
    ~p_update:update_parent
    (get_type w)
    (make_root w)
    (fun _ _ -> false)
    (fun _ -> ())


let rec fill_in_path cells node =
  let x,y = node.pos in
    cells.(x).(y) <- Verboten;
    if node != node.parent then fill_in_path cells node.parent


let gen_v1 seed costs moves x y obst_p =
  let board = make_empty_instance costs moves x y in
  let sface = df_iface seed board in
    Verb.pe Verb.always "Solving Blank using dfs...\n";
  let (s,_,_,_,_,_) = Depth_first_search.dups_hash_firstsol sface [||] in
    Verb.pe Verb.always "Solved!\n";
  let cells = Array.create_matrix x y Free in
    (match s with
	 None -> failwith "Not possible"
       | Some (p,_) -> fill_in_path cells p);
    Verb.pe Verb.always "Traced path\n";
    for x = 0 to (x - 1)
    do
      for y = 0 to (y - 1)
      do
	(match cells.(x).(y) with
	   | Verboten -> cells.(x).(y) <- Free
	   | _ -> (if (Random.float 1.) > obst_p
		   then cells.(x).(y) <- Free
		   else cells.(x).(y) <- Blocked))
      done
    done;
    let blocked =
      Array.map (fun ar ->
		   Array.map (fun e -> match e with
				| Free -> false
				| Blocked -> true
				| _ -> failwith "Not Possible") ar) cells in
      { board with blocked = blocked }


let solvable on_path b board =
  let problem = {b with blocked = (Wrarray.map_matrix board
				     (fun ele -> match ele with
					  Blocked -> true
					| _ -> false))} in
  let iface = Grid_interfaces.default_interface problem [] in
    match Speedy.drop_dups iface [||] with
	(None,_,_,_,_,_) -> (Verb.pe Verb.always "Cannot occlude\n";
			     false)
      | (Some (p,f),_,_,_,_,_) ->
	  (Verb.pe Verb.always "Generating new path, %f\n" f;
	   Hashtbl.clear on_path;
	   let rec add_node p =
	     Hashtbl.add on_path p.pos true;
	     if p == p.parent then true
	     else add_node p.parent in
	     if Math.finite_p f
	     then add_node p
	     else false)


let gen_v2 seed costs moves maxx maxy obst_p =
  let board = make_empty_instance costs moves maxx maxy in
  let sface = df_iface seed board in
    Verb.pe Verb.always "Solving Blank using dfs...\n";
    let (s,_,_,_,_,_) = Depth_first_search.dups_hash_firstsol sface [||] in
      Verb.pe Verb.always "Solved!\n";
      let cells = Array.create_matrix maxx maxy Free
      and to_add = ref []
      and on_path = Hashtbl.create 3000 in
	(match s with
	     None -> failwith "Not possible"
	   | Some (p,_) -> fill_in_path cells p);
	Verb.pe Verb.always "Traced path\n";
	for x = 0 to (maxx - 1) do
	  for y = 0 to (maxy - 1) do
	    (if y = 0 && (x = 0 || x = (maxx - 1))
	     then cells.(x).(y) <- Free
	     else (if (Random.float 1.) < obst_p
		   then (match cells.(x).(y) with
			   | Verboten -> to_add := (x,y)::!to_add
			   | _ -> cells.(x).(y) <- Blocked)
		   else cells.(x).(y) <- Free))
	  done
	done;
	List.iter (fun (x,y) ->
		     let test_board = Wrarray.copy_matrix cells in
		       test_board.(x).(y) <- Blocked;
		       if solvable on_path board test_board
		       then (cells.(x).(y) <- Blocked;
			     to_add := List.filter
			       (fun (x,y) ->
				  let t_val = Hashtbl.mem on_path (x,y) in
				    if not t_val then cells.(x).(y) <- Blocked;
				  t_val) !to_add)
		       else cells.(x).(y) <- Free) !to_add;
	let blocked =
	  Array.map (fun ar ->
		       Array.map (fun e -> match e with
				    | Free -> false
				    | Blocked -> true
				    | _ -> failwith "Impossible") ar)
	    cells in
	  { board with blocked = blocked }

let gen = gen_v1


(* EOF *)
