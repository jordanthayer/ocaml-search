(**

   Generates orc planning problems

*)

let make_board os p_blocked min_cost max_cost x_size y_size = 
  let new_inst = 
    {

      Orc.cells = Array.init y_size 
	(fun _ -> 
	   Array.init x_size 
	     (fun _ -> 
		if(Random.float 1.0 < p_blocked) then
		  nan
		else
		  Wrrandom.randbetween_f min_cost max_cost
	     )
	);
      Orc.x_size = x_size;
      Orc.y_size = y_size;
      Orc.min_cost = min_cost;
      Orc.max_cost = max_cost;
      Orc.p_blocked = p_blocked;
    }
  in 
    Orc.print_board new_inst os


let make_board_unit os p_blocked min_cost max_cost x_size y_size = 
  let new_inst = 
    {

      Orc.cells = Array.init y_size 
	(fun _ -> 
	   Array.init x_size 
	     (fun _ -> 
		if(Random.float 1.0 < p_blocked) then
		  nan
		else
		  1.0
	     )
	);
      Orc.x_size = x_size;
      Orc.y_size = y_size;
      Orc.min_cost = min_cost;
      Orc.max_cost = max_cost;
      Orc.p_blocked = p_blocked;
    }
  in 
    Orc.print_board new_inst os



let make_board_biased_3 os p_blocked min_cost max_cost x_size y_size = 
  let biased_rf ix = 
    let part = (float_of_int ix) /. (float_of_int x_size) in
      if(part < 0.33) then Wrrandom.randbetween_f min_cost max_cost
      else if (part < 0.66) then 
	Wrrandom.randbetween_f (Wrrandom.randbetween_f min_cost
				max_cost) max_cost
      else 
	Wrrandom.randbetween_f(Wrrandom.randbetween_f (Wrrandom.randbetween_f min_cost
						     max_cost)
			       max_cost) max_cost in

  let new_inst = 
    {

      Orc.cells = Array.init y_size 
	(fun _ -> 
	   Array.init x_size 
	     (fun ix -> 
		if(Random.float 1.0 < p_blocked) then
		  nan
		else
		  biased_rf ix
	     )
	);
      Orc.x_size = x_size;
      Orc.y_size = y_size;
      Orc.min_cost = min_cost;
      Orc.max_cost = max_cost;
      Orc.p_blocked = p_blocked;
    }
  in 
    Orc.print_board new_inst os



let make_ofn name p_blocked min_cost max_cost x_size y_size = 
  let oc = open_out name in
    make_board oc p_blocked min_cost max_cost x_size y_size;
    close_out oc

let make_ofn3 name p_blocked min_cost max_cost x_size y_size = 
  let oc = open_out name in
    make_board_biased_3 oc p_blocked min_cost max_cost x_size y_size;
    close_out oc

let make_ofn_unit name p_blocked min_cost max_cost x_size y_size = 
  let oc = open_out name in
    make_board_unit oc p_blocked min_cost max_cost x_size y_size;
    close_out oc


let path_root = User_paths.instance_root


let path domain_name domain_attrs instance_name =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    ((domain_attrs
      @ ["num", instance_name;]))


let make_instances_3  p_blocked min_cost max_cost x_size y_size n_instances = 
  for i = 0 to n_instances - 1
  do

    let attrs = [
      "model","tripartite";
      "rows",string_of_int x_size;
      "cols",string_of_int y_size;
      "prob",string_of_float p_blocked;
      "cost_range",string_of_float max_cost;
    ] in 
    let instance_name = path "orc" attrs (string_of_int i) in
      
      make_ofn3 instance_name p_blocked min_cost max_cost x_size y_size;
  done


let make_instances_unit  p_blocked min_cost max_cost x_size y_size n_instances = 
  for i = 0 to n_instances - 1
  do

    let attrs = [
      "model","unit";
      "rows",string_of_int x_size;
      "cols",string_of_int y_size;
      "prob",string_of_float p_blocked;
      "cost_range",string_of_float max_cost;
    ] in 
    let instance_name = path "orc" attrs (string_of_int i) in
      
      make_ofn_unit instance_name p_blocked min_cost max_cost x_size y_size;
  done
