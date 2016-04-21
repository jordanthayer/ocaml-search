(** A set of inadmissible heuristics for the dynamic robot domain *)

open Drn_instance

let corner_cost = 1.
and corner_dist = 1.

let make_slowdown_h problem =
  let get_h_corners = Non_dynamic.make_get_distance_corners problem
  and get_d_corners = Non_dynamic.make_get_d_corners problem in
  let admiss_h = (fun node ->
		    let s = node.Dynamic.state in
		      (fst (get_h_corners s.x s.y)) /. max_speed)
  and admiss_d = (fun node ->
		    let s = node.Dynamic.state in
		      float (fst (get_d_corners s.x s.y)))

  and approx_h = (fun node ->
		    let s = node.Dynamic.state in
		    let len,corn = get_h_corners s.x s.y in
		      (len +. (corner_cost *. (float_of_int corn))) /.
			max_speed)

  and approx_d = (fun node ->
		    let s = node.Dynamic.state in
		    let len,corn = get_d_corners s.x s.y in
		      (((float_of_int len) +.
			  corner_dist *. (float_of_int corn)))) in
    (fun n -> admiss_h n, admiss_d n), approx_h, approx_d


let make_slowdown_heuristics problem =
  let get_h_corners = Non_dynamic.make_get_distance_corners problem
  and get_d_corners = Non_dynamic.make_get_d_corners problem in
  let admiss_h = (fun node ->
		    let s = node.Dynamic.state in
		      (fst (get_h_corners s.x s.y)) /. max_speed)
  and admiss_d = (fun node ->
		    let s = node.Dynamic.state in
		      float (fst (get_d_corners s.x s.y)))

  and approx_h = (fun node ->
		    let s = node.Dynamic.state in
		    let len,corn = get_h_corners s.x s.y in
		      (len /. (max_speed /. (float_of_int (corn+1)))))

  and approx_d = (fun node ->
		    let s = node.Dynamic.state in
		    let len,corn = get_d_corners s.x s.y in
		      (((float_of_int len) +.
			  corner_dist *. (float_of_int corn)))) in
    (fun n -> admiss_h n, admiss_d n), approx_h, approx_d


let slowdown_percent p problem =
  let h = Dynamic.make_approx_time_h problem
  and d = Dynamic.make_approx_d problem
  and hd = Dynamic.hd problem in
  hd, (fun n -> (h n) /. (1. -. p)),
  d


(**************************** Interfaces *****************************)

let slowdown_interface problem lim =
  let problem = expand_obstacles problem in
  let hd,h,d = make_slowdown_h problem in
    (Search_interface.make
       ~h:h
       ~d:d
       ~hd:hd
       ~domain_expand:Dynamic.expand
       ~key:Dynamic.key
       ~key_print:Dynamic.key_to_string
       ~goal_p:Dynamic.goal_p
       ~halt_on:lim
       ~p_update:Dynamic.update_parent
       (*~get_sol_length: *)
       ~equals:(fun a b -> a = b)
       Search_interface.Robot
       (Dynamic.make_root problem)
       (fun _ _ -> false)
       (fun _ -> ()))


let mirror_interface problem lim =
  let problem = expand_obstacles problem in
  let _,h,_ = make_slowdown_h problem in
    (Search_interface.make
       ~h:h
       ~d:h
       ~hd:(fun n -> let v = h n in v,v)
       ~domain_expand:Dynamic.expand
       ~key:Dynamic.key
       ~key_print:Dynamic.key_to_string
       ~goal_p:Dynamic.goal_p
       ~halt_on:lim
       ~p_update:Dynamic.update_parent
       (*~get_sol_length: *)
       ~equals:(fun a b -> a = b)
       Search_interface.Robot
       (Dynamic.make_root problem)
       (fun _ _ -> false)
       (fun _ -> ()))


let slowdown_percent_interface p problem lim =
  let problem = expand_obstacles problem in
  let hd,h,d = slowdown_percent p problem in
    (Search_interface.make
       ~h:h
       ~d:d
       ~hd:hd
       ~domain_expand:Dynamic.expand
       ~key:Dynamic.key
       ~key_print:Dynamic.key_to_string
       ~goal_p:Dynamic.goal_p
       ~halt_on:lim
       ~p_update:Dynamic.update_parent
       (*~get_sol_length: *)
       ~equals:(fun a b -> a = b)
       Search_interface.Robot
       (Dynamic.make_root problem)
       (fun _ _ -> false)
       (fun _ -> ()))



let round_interface problem lim =
  let problem = expand_obstacles problem in
  let hd,h,d = make_slowdown_heuristics problem in
    (Search_interface.make
       ~h:h
       ~d:d
       ~hd:hd
       ~domain_expand:Dynamic.expand
       ~key:Dynamic.key
       ~key_print:Dynamic.key_to_string
       ~goal_p:Dynamic.goal_p
       ~halt_on:lim
       ~p_update:Dynamic.update_parent
       (*~get_sol_length: *)
       ~equals:(fun a b -> a = b)
       Search_interface.Robot
       (Dynamic.make_root problem)
       (fun _ _ -> false)
       (fun _ -> ()))


(* EOF *)
