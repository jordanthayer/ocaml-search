(* builds a grid designed to demonstrate the f-shadowing behavior *)

open Grid_instance

type bouding_box = {
  up_left: int * int;
  low_right: int * int;
}

let in_bbox x y bb =
  let ux,uy = bb.up_left
  and lx,ly = bb.low_right in
    x >= ux && x <= lx && y <= ly && y >= uy

let print_bbox bb =
  let ux,uy = bb.up_left
  and lx,ly = bb.low_right in
  Verb.pe Verb.toplvl "%d,%d to %d,%d\n%!" ux uy lx ly

let bbox_uniform_random_blocked width height p_blocked bbox =
  Array.init width
    (fun x ->
       Array.init height (fun y ->
			    if in_bbox x y bbox
			    then Math.true_with_prob p_blocked
			    else false))


let make_uniform_bbox_board width height p_blocked c m =
  (** not necessarily feasible! *)
  (* boards must be designed so that optimal solution is board width *)
  (* dx,dy from up_right -> goal : dx = dy *)

  let w = float_of_int width
  and h = float_of_int height in
  let bbox = { up_left = (width / 2, height / 10);
	       low_right = int_of_float (0.76 *. w), int_of_float (0.9 *. h)
  } in
    print_bbox bbox;
    let blocked = bbox_uniform_random_blocked width height p_blocked bbox in
      make_board blocked c m (0,height/2) ((width-1),height/2)


let feasible_board solver w h p cost_model move_model =
  Wrutils.eval_until (fun () ->
		      Wrutils.pr "trying...%!";
		      make_uniform_bbox_board w h p cost_model move_model)
    (feasible_p solver)
