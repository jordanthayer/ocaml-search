(**

    @author jordan
    @since 2011-05-24

   A special variety of grid instance laid out as a ladder on its side
*)

(*
type board = {
  blocked : bool array array;
  costs : cost_model;
  moves : move_model;
  goal : (int * int) list;
  start : int * int;
  instance: int;
}*)

type ladder = {
  height : int;
  width : int;
  ladder_width : int;
}


let construct_ladder height width ladder_width =
  (** Builds the blocked array that is the problem, more or less *)
  assert (ladder_width <= (height - 4));
  assert ((height mod 2) = 0);
  assert (width > 10);
  let blocked = Array.create_matrix width height false
  and gap = (height - ladder_width) / 2  in
  let upper_rung = height - gap
  and lower_rung = gap in
  assert (gap > 0);
(* put in the ladder sides *)
  Verb.pe Verb.debug "Setting up sides\n%!";
  for i = 1 to (width - 2) do
    (blocked.(i).(upper_rung) <- true;
     blocked.(i).(lower_rung) <- true)
  done;
(* put in the ladder rungs *)
  Verb.pe Verb.debug "Building rungs\n%!";
  for i = 3 to (width - 3) do
    (if (i mod 4) = 0
     then (for j = upper_rung downto (lower_rung + 2) do
	 blocked.(i).(j) <- true
       done)
     else if (i mod 2) = 0
     then (for j = lower_rung to (upper_rung - 2) do
	 blocked.(i).(j) <- true
       done))
  done;
  blocked


let make ?(inum = -1) height width ladder_width =
  let blocked = construct_ladder height width ladder_width in
  let midy = height / 2
  and sx = 1
  and gx = (width - 1) in
  { Grid.blocked = blocked;
    costs = Grid.Unit;
    moves = Grid.Fourway;
    goal = [gx,midy];
    start = sx,midy;
    instance = inum;}


let make_default_lw ?(inum = -1) height width =
  let lw = height / 2 in
  let lw = if lw mod 2 = 0 then lw else lw - 1 in
    make ~inum height width lw


let save = Grid_instance.save

(* EOF *)
