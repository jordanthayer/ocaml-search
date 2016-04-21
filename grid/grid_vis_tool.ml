(*
 *  Visualization tool
 *)
open Eps
open Grid

let vis_board = ref (Array.make_matrix 0 0 [])
and canvas = ref (Image.create 0 0)

let v_height () =
  Array.length !vis_board.(0)

let v_width () =
  Array.length !vis_board

let init w =
  let ar = Array.make_matrix (width w) (height w) [] in
    for x = 0 to (width w) - 1
    do
      for y = 0 to (height w) -1
      do
	if w.blocked.(x).(y)
	then ar.(x).(y) <- [-1.]
	else ar.(x).(y) <- []
      done
    done;
    vis_board := ar


let set_cell x y value =
  if x < 0 || y < 0 || x >= (Array.length !vis_board) ||  y >= (Array.length !vis_board.(0))
  then failwith (Wrutils.str "%d,%d out of bounds\n%!" x y)
  else
    !vis_board.(x).(y) <- value :: !vis_board.(x).(y)


let incr_cell x y =
  if x < 0 || y < 0 || x >= (Array.length !vis_board) ||  y >= (Array.length !vis_board.(0))
  then failwith (Wrutils.str "%d,%d out of bounds\n%!" x y)
  else
    (match !vis_board.(x).(y) with
	 [] -> !vis_board.(x).(y)  <- [1.]
       | [ele] -> !vis_board.(x).(y) <- if ele < 0. then [0.] else [ele +. 1.]
       | _ -> failwith "unmached")



let rec draw_sol s =
  match s with
      (x,y)::tl -> (!vis_board.(x).(y) <- [-. 3.];
		    draw_sol tl)
    | [] -> ()


let print_sol returned =
  match returned with
      Some(sol,cst),_,_,_,_,_  -> (draw_sol sol;
				   cst)
    | _ -> -1.


let vis_expand w vis_array =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w
  and it = ref 0. in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos
       and px, py = n.parent.pos in
       let cost = costs.(ny)
       and children = ref [] in
	 it := !it +. 1.;
	 set_cell nx ny !it;
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if (((x != px) || (y != py)) &&
			    (x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
			  Wrutils.push ({ pos = (x,y);
					parent = n; },
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 !children)



let vis_count_expand w vis_array =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos
       and px, py = n.parent.pos in
       let cost = costs.(ny)
       and children = ref [] in
	 incr_cell nx ny;
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if (((x != px) || (y != py)) &&
			    (x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
			  Wrutils.push ({ pos = (x,y);
					parent = n; },
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 !children)


let vis_fun_expand w vis_array f =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos
       and px, py = n.parent.pos in
       let cost = costs.(ny)
       and children = ref [] in
	 set_cell nx ny (f g n);
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if (((x != px) || (y != py)) &&
			    (x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
			  Wrutils.push ({ pos = (x,y);
					parent = n; },
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 !children)

let make_vis_expand w =
  (* add verbosity and caching according to the dynamic environment *)
  init w;
  let e = ref (vis_expand w !vis_board) in
    Verb.force 5 (lazy (e := make_verbose !e));
    make_caching !e

let make_vis_count_expand w =
  (* add verbosity and caching according to the dynamic environment *)
  init w;
  let e = ref (vis_count_expand w !vis_board) in
    Verb.force 5 (lazy (e := make_verbose !e));
    make_caching !e


let make_fun_expand w f =
  init w;
  let e = ref (vis_fun_expand w !vis_board f) in
    Verb.force 5 (lazy (e := make_verbose !e));
    make_caching !e


let def_interface w lim =
  let rev_board = reverse_board w in
    Search_interface.make
      ~h:(get_cheapest_h w)
      ~d:(get_cheapest_d w)
      ~hd:(get_cheapest_hd w)
      ~rev_h:(get_cheapest_h rev_board)
      ~rev_d:(get_cheapest_d rev_board)
      ~rev_hd:(get_cheapest_hd rev_board)
      ~domain_expand:(make_expand w)
      ~key:key
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())


let vis_interface w lim =
  let rev_board = reverse_board w in
    Search_interface.make
      ~h:(get_cheapest_h w)
      ~d:(get_cheapest_d w)
      ~hd:(get_cheapest_hd w)
      ~rev_h:(get_cheapest_h rev_board)
      ~rev_d:(get_cheapest_d rev_board)
      ~rev_hd:(get_cheapest_hd rev_board)
      ~domain_expand:(make_vis_expand w)
      ~key:key
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())


let vis_near_interface w lim =
  let rev_board = reverse_board w in
  let h = (get_cheapest_h w)
  and d = (get_nearest_d w) in
    Search_interface.make
      ~h
      ~d
      ~hd:(fun n -> h n, d n)
      ~rev_h:(get_cheapest_h rev_board)
      ~rev_d:(get_cheapest_d rev_board)
      ~rev_hd:(get_cheapest_hd rev_board)
      ~domain_expand:(make_vis_expand w)
      ~key:key
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())


let vis_count_interface w lim =
  let rev_board = reverse_board w in
    Search_interface.make
      ~h:(get_cheapest_h w)
      ~d:(get_cheapest_d w)
      ~hd:(get_cheapest_hd w)
      ~rev_h:(get_cheapest_h rev_board)
      ~rev_d:(get_cheapest_d rev_board)
      ~rev_hd:(get_cheapest_hd rev_board)
      ~domain_expand:(make_vis_count_expand w)
      ~key:key
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())


let vis_fun_interface w f lim =
  let rev_board = reverse_board w in
    Search_interface.make
      ~h:(get_cheapest_h w)
      ~d:(get_cheapest_d w)
      ~hd:(get_cheapest_hd w)
      ~rev_h:(get_cheapest_h rev_board)
      ~rev_d:(get_cheapest_d rev_board)
      ~rev_hd:(get_cheapest_hd rev_board)
      ~domain_expand:(make_fun_expand w f)
      ~key:key
      ~equals:equals
      ~goal_p:(make_goal_p w)
      ~halt_on:lim
      ~get_sol_length:sol_length
      (get_type w)
      (make_root w)
      (fun _ _ -> false)
      (fun _ -> ())


(* d is for default, and thats good enough for me. *)
let d_height = 600
and d_width = 800

let c_height = ref d_height
and c_width = ref d_width
and c_cell = ref 1
and lines_on = ref false

let resize_c v =
  c_cell := v


let init_vis () =
  Verb.pe Verb.toplvl "%d x %d\n%!" (v_width ()) (v_height ());
  canvas := Image.create ~bgcolor:(255,255,255) (v_width ()) (v_height ())


let list_max ls min =
  if ls == []
  then -2.
  else List.fold_right max ls min

let pos_min a b =
  if a < 0.
  then b
  else if b < 0.
  then a
  else min a b

let list_min ls min_v =
  if ls == []
  then -2.
  else List.fold_right pos_min ls min_v

let list_fst ls min_v =
  if ls == []
  then -2.
  else List.hd (List.rev ls)


let max_board () =
  let to_ret = ref neg_infinity in
    for x = 0 to (v_width ()) - 1
    do
      for y = 0 to (v_height ()) - 1
      do
	to_ret := max !to_ret (list_max !vis_board.(x).(y) !to_ret)
      done
    done;
    !to_ret


let min_board ()=
  let to_ret = ref infinity in
    for x = 0 to (v_width ()) - 1
    do
      for y = 0 to (v_height ()) - 1
      do
	let next = list_min !vis_board.(x).(y) !to_ret in
	  if next > 0.
	  then to_ret := next
      done
    done;
    !to_ret


(* -1 for blocked
   -2 for unexplored
   -3 for goal *)

let rec get_color v1 max_v =
  let v1 = int_of_float v1 in
  if v1 = -1
  then (0, 0, 0)
  else (if v1 = -2
	then (255, 255, 255)
	else (if v1 = -3
	      then (0, 255, 0)
	      else
    (* from 125 0   0
       to   255 255 125 *)
		if v1 <= 125
		then (255, 255, (125 - v1))
		else if v1 <= 380
		then (255, (255 - (v1 - 125)), 0)
		else ((255 - (v1 - 380)), 0, 0)))


let massage_data ?(before = 0.) ?(after = infinity) ?(range = 510.) ()=
  let max = max_board ()
  and min = min_board () in
  let max = max -. min in
  for x = 0 to (v_width ()) - 1
  do
    for y = 0 to (v_height ()) - 1
    do
      !vis_board.(x).(y) <-
	List.map
	(fun q ->
	   if q < 0.
	   then q
	   else if q < before || q > after
	   then -1.
	   else (q -. min) /. max *. range)
	!vis_board.(x).(y)
    done
  done


let draw ?(before = 0.) ?(after = infinity) ?(scale = 1.0) out =
  let max_expand =  max_board() in
    massage_data ~before:before ~after:after ();
    for x = 0 to (v_width ()) - 1
    do
      for y = 0 to (v_height ()) - 1
      do	(* if the cell wasn't touched, draw it white
		   otherwise, produce a smooth gradient. *)
	let c = get_color (list_max !vis_board.(x).(y) (-3.)) max_expand
	in
	  Image.draw_point !canvas x y c
      done
    done;
    Image.export "eps" ~scale:scale !canvas out


let draw_min ?(before = 0.) ?(after = infinity) ?(scale = 1.0) out =
  let max_expand =  max_board() in
    massage_data ~before:before ~after:after ();
    for x = 0 to (v_width ()) - 1
    do
      for y = 0 to (v_height ()) - 1
      do	(* if the cell wasn't touched, draw it white
		   otherwise, produce a smooth gradient. *)
	let c = get_color (list_fst !vis_board.(x).(y) (infinity)) max_expand
	in
	  Image.draw_point !canvas x y c
      done
    done;
    Image.export "eps" ~scale:scale !canvas out


let draw_fixed ?(before = 0.) ?(after = infinity) ?(scale = 1.0) out =
    for x = 0 to (v_width ()) - 1 do
      for y = 0 to (v_height ()) - 1 do
	(* if the cell wasn't touched, draw it white
	   otherwise, produce a smooth gradient. *)
	let lst = !vis_board.(x).(y) in
	let c =
	  match lst with
	    | [] -> 255,255,255
	    | vl::tl ->
		let vl = List.hd (List.rev lst) in
		if vl = (-1.)
		then 0,0,0
		else if vl = 1.
		then 255,0,0
		else if vl = 2.
		then 0,255,0
		else if vl = 3.
		then 0,0,255
		else failwith (Printf.sprintf "Unrecognized value: %f" vl) in
	  Image.draw_point !canvas x y c
      done
    done;
    Image.export "eps" ~scale:scale !canvas out

(* EOF *)
