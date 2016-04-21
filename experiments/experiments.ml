(**
   Info for running experiments.
*)

include User_paths

(** Feasible_p's are used to validate new problems **)

let default_feasible_time = 60.

let feasible search sh ib =
  let fn = (Alg_initializers.string_array_init search sh ib [||])
  in
    (fun problem -> fn problem [(Limit.Time default_feasible_time)])


let feasible_p  solution_handler d_interface =
  (** Feasible p for problems without dups *)
  feasible Speedy.no_dups solution_handler d_interface


let feasible_p_dups solution_handler d_interface =
  feasible Speedy.dups solution_handler d_interface


(* Basic Schedules and limits *)

and anytime_weights = [ 5.; (*3.; 2.;*)]

and low_res_weights = [ 5.; 4.; 3.; 2.; 1.75; 1.5; 1.2; 1.1; 1.01; 1.0;]

and full_weights =   [ 100000.; 100.; 50.; 20.; 15.; 10.; 7.; 5.; 4.; 3.;
		       2.5; 2.; 1.75; 1.5; 1.3; 1.2; 1.15; 1.1;1.05; 1.01;
		       1.001; 1.0005; 1.0;]

let rev_lowres = List.rev low_res_weights

and rev_weights = List.rev full_weights

(* bounded cost cost sets *)
let bc_tiles = [100.; 20.; 200.; 30.; 300.; 400.; 40.; 50.; 500.; 60.; 70.; 80.; 90.;]
let inv_tiles = [20.; 17.; 15.; 14.; 13.; 12.; 11.; 10.; 5.; 3.; 2.; 1.;]
and bc_dock_robot = [10.;  15.;  20.;  25.;  30.;  35.;  40.;  45.;  50.;
		     75.; 100.; 150.;]
and bc_vacuum = [1000.; 2000.; 3000.;  3500.;  4000.;  5000.; 6000.; 7000.;
		 8000.; 9000.; 10_000.; 15_000.; 20_000.; 30_000.]
and bc_grid = [1_000_000.; 1_250_000.; 1_500_000.; 2_000_000.; 1_750_000.;
	       2_100_000.;  2150000.;  2200000.;  2250000.;  2300000.;
	       2350000.;  2400000.; 2_750_000.; 3_000_000.]

and bc_drn = [50.; 45.; 40.; 35.; 30.; 25.; 20.; 15.; 10.; 5.; 4.; 3.; 2.; 1.]

and bc_zeno = [50.; 45.; 40.; 35.; 30.; 25.; 20.; 15.; 10.; 5.]
and bc_zeno_big = [200.; 175.; 150.; 125.; 100.; 90.; 80.; 70.; 60.;]
and bc_elevataor = [1000.; 900.; 800.; 700.; 600.; 500.; 400.; 300.; 200.; 100.]
and bc_elevataor_big = [2000.; 3000.; 4000.; 5000.; 6000.; 7000.; 8000.; 9000.; 10_000.;]
and cutoffs = [ 5.0; 4.0; 3.5; 3.0; 2.5; 2.0; 1.75; 1.5; 1.3; 1.2; 1.1; 1.05;
		1.03; 1.01; 1.0]


let coo_tiles = [5.; 10.; 20.; 30.; 40.; 50.; 100.; 200.;]
let coo_inv_tiles = [2.; 3.; 5.; 10.; 20.; 30.; 40.; 50.;]
let coo_vacuum = [100.; 500.; 1000.; 2000.; 3000.; 5000.; 10000.; 20000.;]

let ks = [1; 3; 5;]

let full_beams =
  [3;
   5;
   10;
   50;
   100;
   500;
   1000;
   5000;
   10000;
   50000;]

let tile_7_beams =
  [3;
   5;
   10;
   50;
   100;
   500;
   1000;
   5000;
   10000;]


let reduced_beams =
  [3;
   10;
   100;
   1000;
   10000;]


let optimisms = [1.5; 2.; 2.5; 3.; 5.;]
(*let optimisms = [2.;]*)

let default_time_limit = 300.
and default_node_limit = 100000000
and default_walltime_limit = 300.

(* EOF *)
