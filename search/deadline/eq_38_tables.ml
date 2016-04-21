(**

    @author jtd7
    @since 2011-01-13
*)

let base = ""
let tiles = base ^ "wtiles/"
let grids = base ^ "grids/"
let robot = base ^ "dyn_robots/"
let ugrid = grids ^ "ugrids/"
let lgrid = grids ^ "lgrid/"


let get_path domain contract =
  let dir = (match domain with
	       | Search_interface.Tiles -> tiles
	       | Search_interface.Robot -> robot
	       | Search_interface.UGrid -> ugrid
	       | Search_interface.LGrid -> lgrid
	       | _ -> failwith "No Tables For this Domain") in
   dir ^ (string_of_int contract)


let bfactor domain =
  (match domain with
     | Search_interface.Tiles -> 2.1304
     | Search_interface.Robot -> 120.
     | Search_interface.UGrid -> 2.6
     | Search_interface.LGrid -> 2.6
     | Search_interface.Dock_robot -> 4.
     | _ -> 0.)


let depth_est domain =
  (match domain with
     | Search_interface.Tiles -> 40. (* unit *)
	 (* wted tiles *) (*70.*)
     | Search_interface.Robot -> 15.
     | Search_interface.UGrid -> 2200.
     | Search_interface.LGrid -> 6500.
     | Search_interface.Dock_robot -> 70.
     | _ -> 0.)


let nodes_min domain =
  match domain with
    | Search_interface.Tiles -> 2_500_000.
    | Search_interface.UGrid
    | Search_interface.LGrid -> 6_250_000.
    | Search_interface.Robot -> 1_350_000.
    | Search_interface.Dock_robot -> 1_800_000.
    | _ -> -1.

let write_row file depth c array =
  let path = Printf.sprintf "%s_%i_%i.prob" file depth c in
    Wrio.with_outfile path
      (fun ch -> let out  = Printf.fprintf ch "%i\n" in
	 Array.iter out array)


let read_row file =
  Array.of_list
    (Wrio.map_file_lines float_of_string file)


let read_elt file index =
  let ret = ref 0. in
    Wrio.with_file_line file (fun s -> ret := float_of_string s) index;
    !ret


let max_c_at_depth bfactor d = truncate (ceil (bfactor ** (float d)))
let max_r_at_depth contracted_nodes d =  contracted_nodes - d
let n_as_cindex bfactor n = truncate
  (ceil
     ((ceil ((float n) *. bfactor)) /. bfactor))


let get_kl domain contract =
  let path_base = get_path domain contract in
  let max_d = truncate (1.5 *. (depth_est domain)) in
  let vals = Array.create (max_d + 1) 0 in
  let r = ref contract
  and last_n = ref 1 in
    for depth = 0 to max_d do
      (let this_row = Printf.sprintf "%s_%i_%i" path_base depth !last_n in
       let info, array = ((Wrio.with_infile this_row Marshal.from_channel) :
			    (Packed_ints.info * Packed_ints.t)) in
       let get = Packed_ints.make_get info array in
	 Verb.pe Verb.always "Array depth %i: %i by %i loading c: %i, r: %i%!"
	   depth (Array.length array) (Array.length array) !last_n !r;
	 let n = get !r in
	   last_n := n;
	   Verb.pe Verb.always " n: %i\n%!" n;
	   r := max (!r - n) 0;
	   vals.(depth) <- n)
    done;
    Verb.pe Verb.always "Table loaded\n%!";
    vals


let solve_38 domain success_prob goal_prob bfactor depth_est contracted_nodes =
  let max_c_at_depth = (max_c_at_depth bfactor)
  and max_r_at_depth = (max_r_at_depth contracted_nodes) in
  let outfile = get_path domain contracted_nodes in
  let best_n = ref 0
  and best_v = ref 0.
  and n = ref 0
  and this_layer = ref [||]
  and prev_layer = ref [||] in
    for depth = depth_est downto 0 do
      (prev_layer := !this_layer;
       let max_c = max_c_at_depth depth
       and max_r = max_r_at_depth depth in
       let max_c = if max_c <= 0 then max_r else Math.imin max_c max_r in
       let max_c_index = truncate (ceil ((float max_c) /. bfactor)) in
       let goal_p = goal_prob depth in
	 Verb.pe Verb.toplvl "depth @ %i, c %i r %i, goal_p %f\n%!"
	   depth max_c max_r goal_p;
	 this_layer := Array.init (max_c_index + 1) (fun _ -> [||]);
	 for ci = max_c_index downto 1 do
	   (let info = Packed_ints.info (max_r + 1) in
	    let these_ns = Packed_ints.make_create info (max_r + 1) in
	    let set = Packed_ints.make_set info these_ns in
	    let c = truncate (ceil ((float ci) *. bfactor)) in
	      !this_layer.(ci) <- Array.create (max_r + 1) 0.;
	      for r = max_r downto 0 do
		(best_n := 1;
		 best_v := -1.;
		 n := 1;
		 while !n < (min c r) && !best_v < 1. do
		   (let v =
		      Math.fmin 1.
			(if depth = depth_est
			 then (success_prob depth !n) *. goal_p
			 else
			   ((success_prob depth !n ) *.
				(goal_p +.
				     (try
					!prev_layer.(!n).(r - !n)
				      with _ -> 0.)))) in
		      if v > !best_v then (best_n := !n; best_v := v);
		      n := !n + 1)
		 done;
		 !this_layer.(ci).(r) <- !best_v;
		 set r !best_n)
	      done;
	      let out = Printf.sprintf "%s_%i_%i" outfile depth ci in
		Wrio.with_outfile out
		  (fun ch -> Marshal.to_channel ch (info, these_ns) []))
	 done)
    done

(* EOF *)
