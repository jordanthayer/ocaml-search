

let path_root = User_paths.instance_root

let get_glued rows cols = 
  match (rows, cols) with 
      (7,7) -> 
	Wrlist.random_elt [
	  8;9;10;11;12;
	  15;16;17;18;19;
	  22;23;24;25;26;
	  29;30;31;32;33;
	  36;37;38;39;40;
	]
    | (6,6) ->
	Wrlist.random_elt [
	  7;8;9;10;
	  13;14;15;16;
	  19;20;21;22;
	  25;26;27;28;
	]
    | (6,7) -> 
	Wrlist.random_elt [
	  8;9;10;11;12;
	  15;16;17;18;19;
	  22;23;24;25;26;
	  29;30;31;32;33;
	]
    | (5,5) ->
	Wrlist.random_elt [
	  6;7;8;
	  11;12;13;
	  16;17;18;
	]
    | (4,5) ->
	Wrlist.random_elt [
	  6;7;8;
	  11;12;13;
	]
    | (4,4) ->
	Wrlist.random_elt 
	  [5;6;9;10;]
    | _ -> failwith "fill in the table here"

let path domain_name domain_attrs pdb_name =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    ((domain_attrs
      @ ["num", pdb_name;]))


let print_problem ?(glued=[]) rows cols arr ch = 
  assert ((rows * cols) = (Array.length arr));
  Printf.fprintf ch "%d %d\n" rows cols;
  Printf.fprintf ch "starting positions for each tile:\n";
  for i = 0 to (rows * cols) - 1
  do
    Printf.fprintf ch "%d\n" arr.(i);
  done;
  Printf.fprintf ch "goal positions:\n";
  for i = 0 to (rows * cols) - 1
  do
    Printf.fprintf ch "%d\n" i;
  done;
  match glued with 
      [] -> ()
    | lst -> (
	Printf.fprintf ch "glued: ";
	Wrlist.fprint ch string_of_int " " glued;
      )


let rec get_ints ?(exclude=[]) max_range = 
  assert (max_range > 1);
  let first_int = Random.int max_range in
  let second_int = Random.int max_range in
    if (List.mem first_int exclude) then 
      (get_ints ~exclude:exclude max_range)
    else if (List.mem second_int exclude) then 
      (get_ints ~exclude:exclude max_range)
    else if (first_int != second_int && first_int != 0 && second_int
	!= 0) then
      (first_int, second_int)
    else
      get_ints ~exclude:exclude max_range




let make_tile_problem ?(exclude=[]) n_tiles = 
  let to_return = Array.init n_tiles Fn.identity in
    for i = 0 to 9999 do
      let (i1,i2) = get_ints ~exclude:exclude n_tiles in
	Wrarray.swap to_return i1 i2;
    done;
    to_return


let make_instances 
    ?(start_id = 0)
    ?(n_exclude=0) ?(instance_names = None) rows cols n_instances = 
  assert (n_exclude < 2);
  for i = start_id to start_id + n_instances - 1
  do
    let model_type = 
      match instance_names with 
	  None -> 
	    (
	      match n_exclude with
		  0 -> "random"
		| _ -> "glued")
	| Some n -> n in 
    let attrs = [
      "model",model_type;
      "rows",string_of_int rows;
      "cols",string_of_int cols;
    ] 
    in
    let instance_name = path "tiles_instances" attrs 
      (string_of_int i) in

    let to_exclude = match n_exclude with
	0 -> []
      | 1 -> [get_glued rows cols]
      | _ -> failwith "trying to glue too many tiles" in 
    let new_instance = make_tile_problem ~exclude:to_exclude (rows*cols) in
    let outch = open_out instance_name in
      print_problem ~glued:to_exclude rows cols new_instance outch;
      close_out outch;

  done



let make_backward_rw_data n_steps = 
  for i = 0 to 100 do

    let ti = Sliding_tiles_inst.read 
      (open_in
	 "./group/data/tiles_instances/korf/4/4/1") in
    let ti = {ti with Sliding_tiles_inst.initial =
	ti.Sliding_tiles_inst.goal} in
      
    let cti = Sliding_tiles_interfaces.default_interface "unit" ti [] in

      
    let r = Random_walk.extract_result(Random_walk.random_walk cti
					 [|string_of_int n_steps|]) in
    let state_to_inst s i = 
      {i with Sliding_tiles_inst.initial = Packed_ints.unpack
	  (s.Sliding_tiles.contents) 16} in


    let model_type = "random_walk" in
    let attrs = [
      "model",model_type;
      "rows",string_of_int 4;
      "cols",string_of_int 4;
      "length",string_of_int n_steps;
    ] 
    in
    let instance_name = path "tiles_instances" attrs 
      (string_of_int i) in


    let new_prob = state_to_inst r ti in
    let o = open_out instance_name in
      Sliding_tiles_inst.write new_prob o;
      close_out o;
  done
