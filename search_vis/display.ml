(* Total Display *)
open Recorded_run

type 'a displayElement =
  | DpqDisplay of 'a Dpq_display.t
  | GridDisplay of 'a Grid_display.t
  | HashDisplay


type 'a t = {
  run : 'a Run.t;
  elements : 'a displayElement array;
  mutable frame  : int;
  mutable getters : (('a node -> int -> Render.color) * string) array;
  mutable context : int;
  node_printer : 'a -> string;
  mutable scale : Scale.t;
}


let create_scale mins maxs getters sx sy sz =
  { Scale.context = 0;
    Scale.mins = mins;
    Scale.maxs = maxs;
    Scale.names = Array.map (fun (fn,nm) -> nm) getters;
    Scale.loc = sx,sy;
    Scale.leng = sz;
  }

let do_path_display = ref true


(****************************************** Structs ********************)

let get_structs t =
  t.run.Run.run.Recorded_run.structs


let set_getters t getters =
  t.getters <- getters;
  Run.reset_getters t.run (Array.map (fun (fn,nm) -> fn) getters);
  t.scale <- { Scale.context = t.context;
	       Scale.mins = t.run.Run.mins;
	       Scale.maxs = t.run.Run.maxs;
	       Scale.names = (Array.map (fun (fn,nm) -> nm) getters);
	       Scale.loc = t.scale.Scale.loc;
	       Scale.leng = t.scale.Scale.leng}



let run_iter fn t =
  (** iterates the function fn over every element of a
      Recorded_run.t.run, whichi is a hashtable of 'a -> 'a nodes *)
  iter fn t.run.Run.run

let key_to_node t =
  (** macro for get_node in recorded_run *)
 get_node t.run.Run.run


let setup_grid ?(get = [|Render.wrap_index_insensitive_scaler get_g, "g";
			 Render.wrap_index_insensitive_scaler get_depth, "dep";
			 Render.wrap_index_insensitive_scaler get_h, "h";
			 Render.wrap_index_insensitive_scaler get_ht, "h*";
			 Render.wrap_index_insensitive_scaler get_d, "d";
			 Render.wrap_index_insensitive_scaler get_dt, "d*";
			 Render.wrap_index_insensitive_scaler calc_f, "f";
			 Render.wrap_index_insensitive_scaler calc_ft, "f*";
			 Render.wrap_index_insensitive_scaler get_cost, "f'"|])
    ?(str_names = ["focal"; "open"; "cleanup"]) t =
  (** Configures mins and maxs for proper visualizations *)
  let scopes = Array.length get in
  let t =
    { Run.run = t.Run.run;
      Run.problem = t.Run.problem;
      Run.mins = Array.create scopes infinity;
      Run.maxs = Array.create scopes (-.infinity);
      Run.run_leng = t.Run.run_leng} in
    Verb.pe Verb.often "Setting up mins and maxes\n";
    (* iter is Recorded_run.iter, it iterates over the hashtbl run *)
    Array.iteri (fun i (getter,gname) ->
		   match gname with
		     | "g" -> (t.Run.mins.(i) <- 0.;
			       t.Run.maxs.(i) <- t.Run.run.Recorded_run.max_g)
		     | "h" -> (t.Run.mins.(i) <- 0.;
			       t.Run.maxs.(i) <- t.Run.run.Recorded_run.max_h)
		     | "d" -> (t.Run.mins.(i) <- 0.;
			       t.Run.maxs.(i) <- t.Run.run.Recorded_run.max_d)
		     | "depth" -> (t.Run.mins.(i) <- 0.;
				   t.Run.maxs.(i) <- t.Run.run.Recorded_run.max_depth)
(*		     | "f" -> (iter
				 (fun n ->
				    match getter n ~-1 with
				      | Render.Scaler v ->
					  if Math.finite_p v
					  then (if v < t.Run.mins.(i)
						then t.Run.mins.(i) <- v)
				      | _ -> t.Run.mins.(i) <- 0.) t.Run.run;
			       t.Run.maxs.(i) <-
				 t.Run.run.Recorded_run.max_g +.
				 t.Run.run.Recorded_run.max_h)
		     | "f'" -> (iter
				  (fun n ->
				     match getter n ~-1 with
				       | Render.Scaler v ->
					   if Math.finite_p v
					   then (if v < t.Run.mins.(i)
						 then t.Run.mins.(i) <- v)
				       | _ -> t.Run.mins.(i) <- 0.) t.Run.run;
				t.Run.maxs.(i) <-
				  t.Run.run.Recorded_run.max_g +.
				  30. *. t.Run.run.Recorded_run.max_h)*)
		     | "f" -> (t.Run.mins.(i) <- 76.46684;
			       t.Run.maxs.(i) <- 248.847763)
		     | "f'" -> (t.Run.mins.(i) <- 76.46684;
				t.Run.maxs.(i) <- 327.245)
		     | _ -> iter  (fun n ->
				     match getter n ~-1 with
				       | Render.Scaler v ->
					   if Math.finite_p v
					   then (if v < t.Run.mins.(i)
						 then t.Run.mins.(i) <- v;
						 if v > t.Run.maxs.(i)
						 then t.Run.maxs.(i) <- v)
				       | _ -> t.Run.maxs.(i) <- 0.;
					   t.Run.mins.(i) <- 0.) t.Run.run;
			 Verb.pe Verb.often "%s %f %f\n" gname t.Run.mins.(i) t.Run.maxs.(i))
      get;
    Verb.pe Verb.often "Mins and maxes configured\n";
    let gp = match t.Run.problem with Run.GridProblem gp -> gp in
    let gx,gy = gp.Grid_display.lower_left
    and sx,sy = gp.Grid_display.size in

    let rec disp =
      { run = t;
	elements = Array.init (List.length t.Run.run.structs + 1)
	  (fun i ->
	     if i < List.length t.Run.run.structs
	     then DpqDisplay (Dpq_display.new_dispq i
				(List.nth str_names i)
				(List.nth t.Run.run.structs i))
	     else match t.Run.problem with
		 Run.GridProblem gp -> GridDisplay gp);
	frame = 0;
	getters = get;
	context = 0;
	node_printer = (fun (a,b) -> Wrutils.str "(%i , %i)" a b);
	scale = create_scale t.Run.mins t.Run.maxs get
	  (gx + sx * !Constants.grid_squareSize + Constants.offset) sy 300} in
      disp


let updateElement = function
(** Updates a specific dispaly element based on type element *)
  | DpqDisplay dd ->  Dpq_display.draw dd
  | GridDisplay gd -> Grid_display.drawGrid gd
  | _ -> (fun _ _ _ _ -> Verb.pe Verb.always "Not implemented")


let redraw t =
  (** Repaints the display described by [t] from scratch*)
  let fn,lbl = t.getters.(t.context)
  and min = t.run.Run.mins.(t.context)
  and max = t.run.Run.maxs.(t.context)
  and ktn = key_to_node t in
    Grid_display.grid_window#set_title lbl;
    let gc = (fun k -> fn (ktn k)) in (* gc -> get_color *)
      (match
	 (List.rev (Array.to_list t.elements)) with
	     center::structs ->
	     (match center with
		  GridDisplay gd ->
		    let node = ktn t.run.Run.run.sequence.(t.frame) in
		    (Grid_display.clean gd;
		     Grid_display.drawGrid gd gc (t.run.Run.run,t.frame) min max;
		     (Grid_display.overlay_struct gd
			(List.map (fun s -> match s with DpqDisplay d -> d
				     | _ -> failwith "display.ml 144")
			   structs) gc t.frame min max);
		     let gx,gy = gd.Grid_display.lower_left
		     and sx,sy = gd.Grid_display.size in
		       Scale.reset_loc t.scale
			 (gx + sx * !Constants.grid_squareSize + Constants.offset)
			 gy;
		       for i = 0 to (List.length structs - 1)
		       do
			 match List.nth structs i with
			     DpqDisplay dd ->
			       (Dpq_display.draw dd gc (t.run.Run.run, t.frame) min max;)
			   | _ -> failwith "nondisplayable struct"
		       done;
		       if !do_path_display then
		       Grid_display.overlay_path gd gc t.frame min max
			 (Recorded_run.get_path
			    t.run.Run.run.Recorded_run.run node);
		       Grid_display.redraw gd;
		       (* Display current node info and run info now *)
			 Node_display.display_node t.node_printer
			   (ktn t.run.Run.run.sequence.(t.frame)) t.getters t.frame;
			 Run.draw_info t.run t.frame;
			 Scale.draw t.scale;)
		| _ -> failwith "not implemented")
	 | _ -> failwith "Mismatch")


let update_forward t =
  (** Paints the next frame described by [t]
      supposed to be faster than [redraw] *)
  let fn,lbl = t.getters.(t.context)
  and min = t.run.Run.mins.(t.context)
  and max = t.run.Run.maxs.(t.context)
  and ktn = key_to_node t in
    Grid_display.grid_window#set_title lbl;
    let gc = (fun k -> fn (ktn k)) in (* gc -> get_color *)
      (match (List.rev (Array.to_list t.elements)) with
	   center::structs ->
	     (match center with
		  GridDisplay gd ->
		    let prev = Math.imax 0 (t.frame - 1) in
		    let node = ktn t.run.Run.run.sequence.(t.frame)
		    and pnode = ktn t.run.Run.run.sequence.(prev) in
		      (Grid_display.drawStep gd gc (t.run.Run.run, prev) ~color_step:t.frame min max;
		       Grid_display.drawStep gd gc (t.run.Run.run,t.frame) min max;
		       Grid_display.overlay_struct gd
			 (* This is some really bad mojo, go back and fix this
			    later *)
			 (List.map (fun s -> match s with DpqDisplay d -> d
				      | _ -> failwith "display.ml 201") structs) gc t.frame min max;
		       for i = 0 to (List.length structs - 1)
		       do
			 match List.nth structs i with
			     DpqDisplay dd ->
			       (Dpq_display.draw dd gc (t.run.Run.run, t.frame) min max;)
			   | _ -> failwith "nondisplayable struct"
		       done;
		       if !do_path_display then
			 (Grid_display.unlay_path gd gc t.frame min max
			    (Recorded_run.get_path
			       t.run.Run.run.Recorded_run.run pnode);
			  Grid_display.overlay_path gd gc t.frame min max
			    (Recorded_run.get_path
			       t.run.Run.run.Recorded_run.run node));
		       Grid_display.redraw gd;
		       (* Display current node info and run info now *)
		       Node_display.display_node t.node_printer
			 (ktn t.run.Run.run.sequence.(t.frame)) t.getters t.frame;
		       Run.draw_info t.run t.frame)
		| _ -> failwith "not implemented")
	 | _ -> failwith "Mismatch")


let update_backward t =
  (** Paints the next frame described by [t]
      supposed to be faster than [redraw] *)
  let fn,lbl = t.getters.(t.context)
  and min = t.run.Run.mins.(t.context)
  and max = t.run.Run.maxs.(t.context)
  and ktn = key_to_node t in
    Grid_display.grid_window#set_title lbl;
    let gc = (fun k -> fn (ktn k)) in (* gc -> get_color *)
      (match (List.rev (Array.to_list t.elements)) with
	   center::structs ->
	     (match center with
		  GridDisplay gd ->
		    let node = ktn t.run.Run.run.sequence.(t.frame) in
		    let pnode = ktn t.run.Run.run.sequence.(t.frame+1) in
		    let prev = Math.imax 0 (t.frame - 1) in
		      (Grid_display.removeStep gd gc (t.run.Run.run, prev) min max;
		       Grid_display.unlay_struct gd
			 (* This is some really bad mojo, go back and fix this
			    later *)
			 (List.map (fun s -> match s with DpqDisplay d -> d
				      | _ -> failwith "display.ml 243") structs) gc t.frame min max;
		       for i = 0 to (List.length structs - 1)
		       do
			 match List.nth structs i with
			     DpqDisplay dd ->
			       (Dpq_display.draw dd gc (t.run.Run.run, t.frame) min max;)
			   | _ -> failwith "nondisplayable struct"
		       done;
		       if !do_path_display then
			 (Grid_display.unlay_path gd gc t.frame min max
			    (Recorded_run.get_path
			       t.run.Run.run.Recorded_run.run pnode);
			  Grid_display.overlay_path gd gc t.frame min max
			    (Recorded_run.get_path
			       t.run.Run.run.Recorded_run.run node);
			  Grid_display.redraw gd);
		       (* Display current node info and run info now *)
		       Node_display.display_node t.node_printer
			 (ktn t.run.Run.run.sequence.(t.frame)) t.getters t.frame;
		       Run.draw_info t.run t.frame;)
		| _ -> failwith "not implemented")
	 | _ -> failwith "Mismatch")


let stepForward t =
(** Moves forward through time one step *)
  if t.frame <> (t.run.Run.run_leng - 1)
  then (t.frame <- t.frame + 1;
	update_forward t)


let stepBackward t =
(** Steps time backward *)
  if t.frame <> 0
  then (t.frame <- t.frame - 1;
	update_backward t)


let cycle_context t =
  (** Switches the drawing context forward *)
  if t.context <> ((Array.length t.getters) - 1)
  then t.context <- t.context + 1
  else t.context <- 0;
  Scale.set_context t.scale t.context;
  redraw t


let set_context disp context =
  let i = ref (-1) in
    Array.iteri (fun j (fn,nm) -> if nm = context then i := j)
      disp.getters;
    if !i <> -1
    then (disp.context <- !i;
	  Scale.set_context disp.scale disp.context;
	  redraw disp)

(*
let actionloop t =
  (** Interactive visualization loop on [t] *)
  match ActionListener.get_event () with
    | ActionListener.Kill -> failwith "Exited"
    | ActionListener.StepForward -> stepForward t
    | ActionListener.StepBack -> stepBackward t
    | ActionListener.CycleContext -> cycle_context t
    | _ -> ()
*)

(* EOF *)
