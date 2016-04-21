(* Builds Game boards from a given gam beard
   Jordan - Jan 7
 *)

open Grid
open Grid_instance


type bucket_board = {
  opt_sol : float;
  b : board;
}

type bucket_array = {
  bs  : int;
  min : float;
  max : float;
  inc : float;
  bar : bucket_board list array;
}


let make_bar min max incr bs =
  let size = (max - min) / incr in
    {
      bs = bs;
      min = float_of_int min;
      max = float_of_int max;
      inc = float_of_int incr;
      bar = Array.init size (fun n -> []);
    }

let lb ch =
  let w = Wrio.input_int ch in
  let h = Wrio.input_int ch in
    Wrio.skip_through_str ch "Board:\n";
    Verb.pe Verb.toplvl "board %i x %i\n%!" h w;
      let b = Array.init w (fun _ -> Array.make h false) in
	for y = h - 1 downto 0 do
	  for x = 0 to w - 1 do
	    b.(x).(y) <- (input_char ch) = '#'
	  done;
	  Wrio.skip_line ch;
	done;
      { blocked = b;
	costs = Grid.Unit;
	moves = Grid.Eightway;
	goal = [];
	start = -1,-1;
	instance = not_known;}


let load_blocked path =
  Wrio.with_infile path lb


let rec find_random_unblocked brd =
  let x = Random.int ((Array.length brd.blocked) - 1)
  and y = Random.int ((Array.length brd.blocked.(0)) - 1) in
    if not (brd.blocked.(x).(y))
    then (x,y)
    else find_random_unblocked brd


let portion = 6

let rec find_random_start brd =
  let x = Random.int ((Array.length brd.blocked) - 1)
  and y = Random.int (((Array.length brd.blocked.(0)) - 1) / portion) in
    if not (brd.blocked.(x).(y))
    then (x,y)
    else find_random_start brd


let rec find_random_goal brd =
  let x = Random.int ((Array.length brd.blocked) - 1)
  and y = Random.int (((Array.length brd.blocked.(0)) - 1) / portion)
  and maxy = Array.length brd.blocked.(0) - 1 in
    if not (brd.blocked.(x).((maxy - y)))
    then (x,(maxy -y))
    else find_random_goal brd


let rec feasible_board solver brd =
  let b =
    {blocked = brd.blocked;
     costs = Grid.Unit;
     moves = Grid.Eightway;
     goal = [find_random_start brd];
     start = find_random_goal brd;
     instance = not_known;} in
    if not (feasible_p solver b)
    then feasible_board solver brd
    else b


let hard_enough_p solver b =
  failwith "Temporarily broken"
(*
  let (s,e,g,p,m,d) = Grid_algs.a_star b [(Limit.Time 300.)] in
    match s with
	None -> false, 0.
      | Some (n,f) -> true, f*)


let rec hard_enough solver brd =
  let b = feasible_board solver brd in
  let ge, cst = hard_enough_p solver b in
    if ge
    then { opt_sol = cst;
	   b = b}
    else hard_enough solver brd


let bar_insert bar b =
  if b.opt_sol < bar.min || b.opt_sol > bar.max
  then Verb.pe Verb.toplvl "b not acceptable, too high or low\n%!";
  let index = int_of_float ((b.opt_sol -. bar.min) /. bar.inc) in
    if (List.length bar.bar.(index)) < bar.bs
    then bar.bar.(index) <- b::bar.bar.(index)


let load_bucket solver raw_path min_opt max_opt increment bucket_size =
  let bar = make_bar min_opt max_opt increment bucket_size
  and raw_brd = load_blocked raw_path in
  let max_attempts = (Array.length bar.bar) * bucket_size * 10;
  in
    Verb.pe Verb.toplvl "%i max_attempts \n%!" max_attempts;
    for i = 0 to max_attempts
    do
      (if i mod 100 = 0
       then Verb.pe Verb.toplvl "%i of %i\n%!" i max_attempts;
      bar_insert bar (hard_enough solver raw_brd))
    done;
    bar

let save path board_name bar =
  failwith "Not Implemented"

(* EOF *)
