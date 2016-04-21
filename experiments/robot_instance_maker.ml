(** Instance generation for the dynamic robot class of problems *)

let instance_root = Experiments.instance_root ^ "dyn_robot_instances"

let make_liney_instances ?(length = 0.3) height width lines count =
  Random.self_init();
  for i = 1 to count do
    let attrs = ["type", "instance";
		 "obstacles", "liney";
		 "width", string_of_int width;
		 "height", string_of_int height;
		 "num_lines", string_of_int (int_of_float lines);
		 "line_length", string_of_float length;
		 "num", string_of_int i] in
    let path = Rdb.path_for instance_root attrs in
      if Sys.file_exists path then
	Wrutils.pr "%s exists, moving on\n%!" path
      else
	let b = Drn_instance.make_instance
	  (Drn_instance.make_liney length) width height lines in
	  Drn_instance.save path b;
	  Wrutils.pr "Wrote Problem %d.\n%!" i
  done

(* EOF *)
