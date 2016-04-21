(* A fast domain-dependent grid solver.  This is just a uniform cost
   search.  We would hope that our less domain-dependent A* solver can
   beat this. *)

open Grid

(** Build functions for uniquely identifying grid locations. *)
let id_funs brd =
  let h = height brd in
  let loc_id x y = x * h + y in
  let id_loc id = id / h, id mod h in
  loc_id, id_loc

(** Expands nodes directly into the queue. *)
let expand cost eight brd ps gs gend =
  let w = width brd and h = height brd and blkd = brd.blocked in
  let loc_id, id_loc = id_funs brd in
  let enqueue q id g x' y' =
    incr gend;
    let id' = loc_id x' y' in
    let g' = g +. cost id id' in
    if not blkd.(x').(y') && ps.(id) <> id' && g' < gs.(id') then begin
      ps.(id') <- id;
      gs.(id') <- g';
      Dpq.insert q id'
    end in
  let expand id g q' =
    let x, y = id_loc id in
    let top = y = 0 and bottom = y = h - 1 in
    let left = x = 0 and right = x = w - 1 in
    if not top then begin
      enqueue q' id g x (y - 1);
      if eight && not left then
	enqueue q' id g (x - 1) (y - 1);
      if eight && not right then
	enqueue q' id g (x + 1) (y - 1);
    end;
    if not bottom then begin
      enqueue q' id g x (y + 1);
      if eight && not left then
	enqueue q' id g (x - 1) (y + 1);
      if eight && not right then
	enqueue q' id g (x + 1) (y + 1);
    end;
    if not left then enqueue q' id g (x - 1) y;
    if not right then enqueue q' id g (x + 1) y; in
  expand

let costs brd =
  Array.create (width brd * height brd) infinity

(** Uniform cost search.  The resulting path can be traced back via
    the parent pointers. *)
let uniform_cost cost eight ps expd gend brd gx gy x0 y0 =
  let goal = ref infinity in
  let loc_id, _ = id_funs brd in
  let goal_id = loc_id gx gy in
  let gs = costs brd in
  let expand n g q' =
    incr expd;
    expand cost eight brd ps gs gend n g q' in
  let rec search q q' =
    if Dpq.empty_p q then
      infinity
    else begin
      while not (Math.finite_p !goal) && not (Dpq.empty_p q) do
	let n = Dpq.extract_first q in
	let g = gs.(n) in
	if n = goal_id then goal := g else expand n g q'
      done;
      if Math.finite_p !goal then !goal else search q' q
    end in
  let better a b =
    let ga = gs.(a) and gb = gs.(b) in if ga = gb then a < b else ga < gb in
  let q = Dpq.create_with better ~-1 in
  let q' = Dpq.create_with better ~-1 in
  let id0 = loc_id x0 y0 in
  gs.(id0) <- 0.;
  Dpq.insert q id0;
  search q q'

let rec trace_sol id_loc ps id =
  let pid = ps.(id) in
  if pid = id then [id_loc id] else id_loc id :: trace_sol id_loc ps pid

let parents brd =
  Array.create (width brd * height brd) ~-1

let search cost eight brd =
  let expd = ref 0 and gend = ref 0 in
  let gx, gy = List.hd brd.goal in
  let x0, y0 = brd.start in
  let ps = parents brd in
  let loc_id, id_loc = id_funs brd in
  let id0 = loc_id x0 y0 in
  ps.(id0) <- id0;
  let cost = uniform_cost cost eight ps expd gend brd gx gy x0 y0 in
  if Math.finite_p cost then
    Some (trace_sol id_loc ps (loc_id gx gy)), !expd, !gend, cost
  else
    None, !expd, !gend, cost

let read_brd () =
  let file_name = Unix.readlink (Unix.readlink "/dev/stdin") in
  let path = Grid_instance.prob_path_to_instance file_name in
  Grid_instance.read path stdin

let make_unit_cost eight brd =
  let _, id_loc = id_funs brd in
  let sq2 = sqrt 2. in
  if eight then
    (fun id id' ->
      let x, y = id_loc id and x', y' = id_loc id' in
      if x' <> x && y' <> y then sq2 else 1.)
  else
    (fun _ _ -> 1.)

let cost_fun eight brd = match brd.costs with
  | Unit -> make_unit_cost eight brd
  | Life -> invalid_arg "Grid_search.cost_fun: Life is not yet supported"

let main () =
  let brd = read_brd () in
  let eight = brd.moves = Grid.Eightway in
  let cost = cost_fun eight brd in
  let (sol, expd, gend, cost), time =
    Wrsys.with_time (fun () -> search cost eight brd) in
  let sol_found, sol_len = match sol with
    | None -> "no", []
    | Some s -> "yes", ["final sol length", string_of_int (List.length s)] in
  Datafile.write_pairs stdout
    (sol_len @ [ "final sol cost", string_of_float cost;
		 "total nodes expanded", string_of_int expd;
		 "total nodes generated", string_of_int gend;
		 "total raw cpu time", string_of_float time;
		 "found solution", sol_found; ])

let _ = main ()
