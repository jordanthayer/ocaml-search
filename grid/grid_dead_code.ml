(* dead code from grid world

*)



let compare_h ?(w = 70) ?(h = 20) ?(p = 0.3) ?(a = Algs.a_star) () =
  (* compare on life costs *)
  let b = feasible_board life_costs w h p feasible_four_p in
  let do_e e =
    Wrutils.pr "\n--- zero ---\n\n";
    test (a e zero) b;
    Wrutils.pr "\n--- vert ---\n\n";
    test (a e smooth_vertical_cost) b;
    Wrutils.pr "\n--- diag ---\n\n";
    test (a e smooth_diag_cost) b;
    Wrutils.pr "\n--- manhattan ---\n\n";
    test (a e life_manhattan_cost) b
  in
    Wrutils.pr "\n--- with 4-way moves ---\n\n";
    do_e expand_four;
    Wrutils.pr "\n--- with 8-way moves ---\n\n";
    do_e expand_eight



(************************ lower bounds ***********************)


let zero n =
  (** always admissible! *)
  0.


let min_dim_distance n =
  (** admissible in both 4-way and 8-way.  dominated by max_dim_distance *)
  let (x, y) = n.pos
  and (gx, gy) = n.board.goal in
    float (min (abs (gx - x))
	     (abs (gy - y)))


let straight_line_distance n =
  (** not admissible in the 8-move board because diagonal steps <>
    sqrt(2).  dominated by [manhattan_distance] *)
  let (x, y) = n.pos
  and (gx, gy) = n.board.goal in
    sqrt ((Math.square (float (gx - x))) +.
	    (Math.square (float (gy - y))))


(*** cost - assumes "smooth" or "life"-like move costs ***)


let smooth_vertical_cost n =
  (** admissible for both 4- and 8-way boards.  for our boards, only a
    tiny bit better than zero. *)
  cost_from n.board (snd n.pos) (snd n.board.goal)


(***
let cost_up w y1 y2 =
  (* going up from y1 to y2 *)
  let h = float (height w)
  and cost_y1 = cost w y1
  and y1 = float y1
  and y2 = float y2 in
  let c1 = y1 *. (cost_y1 +. h)
  and c2 = y1 +. cost_y1 +. h in
    ((-. (y2 *. y2)) +. (c2 *. y2) -. c1) /. 2.


let cost_down w y2 y1 =
  (* going down from y2 to y1 *)
  let h = float (height w)
  and cost_y1p = (cost w y1) -. 1.
  and y1 = float y1
  and y2 = float y2 in
  let c3 = y1 *. (cost_y1p +. (h -. 1.))
  and c4 = y1 +. cost_y1p +. (h -. 1.) in
    ((-. (y2 *. y2)) +. (c4 *. y2) -. c3) /. 2.


let test_vert_cost n =
  let w = life_board 1 n 0. in
    Wrutils.pr "-- testing cost_up --\n";
    for y1 = 0 to n - 1 do
      for y2 = y1 to n - 1 do
	let oldway = cost_from w y1 y2
	and newway = cost_up w y1 y2 in
	  if oldway <> newway then
	    Wrutils.pr "for %d to %d, old = %f but new = %f\n"
	      y1 y2 oldway newway
      done
    done;
    Wrutils.pr "-- testing cost_down --\n";
    for y1 = n-1 downto 0 do
      for y2 = n-1 to y1 do
	let oldway = cost_from w y2 y1
	and newway = cost_down w y2 y1 in
	  if oldway <> newway then
	    Wrutils.pr "for %d to %d, old = %f but new = %f\n"
	      y2 y1 oldway newway
      done
    done
***)


(********** cost and dist to cheapest goal *************)


let zero_zero _ _ =
  (** cost, dist *)
  0., 0


let smooth_vertical_cost_dist w (_,y) =
  (** admissible for both 4- and 8-way, but rather a severe
    underestimate *)
  let (_,gy) = w.goal in
    cost_from w y gy, abs (y - gy)


(***** cost only ***)


let life_eight_cost n =
  fst (smooth_diag_cost_dist n.board n.pos)


(*** cost in 4-way world ***)


let life_four_cost n =
  fst (life_manhattan_cost_dist n.board n.pos)


let life_four_dist n =
  float (snd (life_manhattan_cost_dist n.board n.pos))



(******************************* main.ml ******************************)




let get_h =
  let h_table = [ "zero", Grid.zero;
		  (* life costs *)
		  "smooth_vertical_cost", Grid.smooth_vertical_cost;
		  "smooth_diag_cost", Grid.smooth_diag_cost;
		  (* not in 8-way *)
		  "life_manhattan_cost", Grid.life_manhattan_cost;
		  (* unit costs *)
		  "min_dim_distance", Grid.min_dim_distance;
		  "max_dim_distance", Grid.max_dim_distance;
		  (* not in 8-way *)
		  "straight_line_distance", Grid.straight_line_distance;
		  "manhattan_distance", Grid.manhattan_distance ] in
    (fun name ->
       Wrlist.get_entry h_table "cost to cheapest goal" name)


let get_d =

  let d_table = [ "zero", Grid.zero;
		  "min_dim_distance", Grid.min_dim_distance;
		  "max_dim_distance", Grid.max_dim_distance;
		  (* not in 8-way *)
		  "straight_line_distance", Grid.straight_line_distance;
		  "manhattan_distance", Grid.manhattan_distance ] in
    (fun name ->
       Wrlist.get_entry d_table "search distance to nearest goal" name)

(*
let get_df =
  let df_table = [ "max_dim_distance", Grid.max_dim_distance;
		   (* not in 8-way *)
		   "life_manhattan_dist", Grid.life_manhattan_dist;
		   (* unit costs *)
		   (* "max_dim" *)
		   (* not in 8-way *)
		   "manhattan_distance", Grid.manhattan_distance; ] in
    (fun name ->
       Wrlist.get_entry df_table "distance to cheapest" name)
*)

let get_hd =
  let hd_table = [ "zero", Grid.zero_zero;
		   (*--for life costs--*)
		   (* to cheapest *)
		   "smooth_vertical_cost_dist", Grid.smooth_vertical_cost_dist;
		   "smooth_diag_cost_dist", Grid.smooth_diag_cost_dist;
		   (* not in 8-way *)
		   "life_manhattan_cost_dist", Grid.life_manhattan_cost_dist;
		   (* to nearest *)
		   "smooth_vertical_cost_dist", Grid.smooth_vertical_cost_dist;
		   "smooth_diag_cost_dist", Grid.smooth_diag_cost_dist;
		   (* not in 8-way *)
		   "manhattan_cost_dist", Grid.manhattan_cost_dist;
		   (*--for unit costs, cheapest=nearest--*)
		   "unit_max_dim_cost_dist", Grid.unit_max_dim_cost_dist;
		   (* not in 8-way *)
		   "unit_manhattan_cost_dist", Grid.unit_manhattan_cost_dist;
		 ] in
    (fun name ->
       Wrlist.get_entry hd_table "cost/distance estimator for cheapest or nearest" name)





(* EOF *)
