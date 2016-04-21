(* Synthetically generates boards with many goals

   Start state is in the center of the board
   Goal states are along the edge

 *)

open Grid
open Grid_instance


let gradient_blocked wide high start_p end_p =
  let delta = (end_p -. start_p) /. (float_of_int wide) in
    Array.init wide
      (fun x ->
	 Array.init high (fun _ ->
			      Math.true_with_prob
				(((float_of_int x) *. delta) +. start_p)))


let make_goals b g_count =
  let h = Array.length b.(0)
  and w = Array.length b in
    Verb.pe Verb.toplvl "%d %d\n%!" h w;
  match g_count with
      1 -> [w-1, h/2;]

    | 2 -> [w-1, h/2;
	    0  , h/2;]

    | 4 -> [w-1, h/2;
	    0  , h/2;
	    w/2, 0;
	    w/2, h-1;]

    | 8 ->  [w-1, h/2;
	     0  , h/2;
	     w/2, 0;
	     w/2, h-1;
	     w-1,h-1;
	     0,0;
	     0,h-1;
	     w-1,0;]

    | _ -> failwith "Goal count not handled.  Manygoal line 32"


let make_board b c m g_count =
  let h = Array.length b.(0)
  and w = Array.length b
  and goals = make_goals b g_count in
    List.iter (fun (x,y) -> b.(x).(y) <- false) goals; (*empties goal states*)
    b.(w/2).(h/2) <- false; (*empty start state *)
    {blocked = b;
     costs = c;
     moves = m;
     goal = goals;
     start = w / 2, h / 2;
     instance = not_known;}


let make_many_goal w h cost move start_p end_p goals =
  let blocked = gradient_blocked w h start_p end_p in
    make_board blocked cost move goals


let feasible_board solver w h cost_model move_model start_p end_p goals =
  Wrutils.eval_until
    (fun () ->
       Wrutils.pr "trying...%!";
       make_many_goal w h cost_model move_model start_p end_p goals)
    (feasible_p solver)


(* EOF *)
