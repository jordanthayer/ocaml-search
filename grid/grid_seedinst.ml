(* Instances where the board is defined meerly by a random seed. *)

open Fn
open Printf
open Scanf

(* Ranq1, from "Numerical Recipes 3rd edition," Press, Tenkolsky,
   Vetterling and Flannery, 2007.  Technically, the algorithm is
   defined on *unsigned* 64-bit integers.  OCaml only seems to have
   signed 64-bit ints.  So, we trudge on hoping that everything will
   be OK.  We only use 30 or 62 bits anyway... *)
let mkrnd seed =
  let mul = 2685821657736338717L in
  let v = ref 4101842887655102017L in
  let next () =
    v := Int64.logxor !v (Int64.shift_right !v 21);
    v := Int64.logxor !v (Int64.shift_left !v 35);
    v := Int64.logxor !v (Int64.shift_right !v 4);
    Int64.abs (Int64.mul !v mul) in
  let intgr () = abs (Int64.to_int (next ())) in
  let fl = 5.42101086242752217e-20 in
  let flt () = Int64.to_float (next ()) *. fl in
  v := Int64.logxor !v (Int64.of_int seed);
  v := next ();
  intgr, flt

let mkblkd w h pr seed =
  let _, f = mkrnd seed in
  Wrarray.init_matrix w h (fun _ _ -> pr < f ())

let read inch =
  let seed = fscanf inch "%d" identity in
  let w, h = fscanf inch " %d %d" gather2 in
  let pr = fscanf inch " %g" identity in
  let move = match fscanf inch " %s\n" identity with
    | "Fourway" -> Grid.Fourway
    | "Eightway" -> Grid.Eightway
    | s -> failwith (sprintf "Invalid movement type: %s" s) in
  let cost = match fscanf inch " %s\n" identity with
    | "Unit" -> Grid.Unit
    | "Life" -> Grid.Life
    | s -> failwith (sprintf "Invalid cost type: %s" s) in
  let gx, gy = fscanf inch " %d %d" gather2 in
  let x0, y0 = fscanf inch " %d %d" gather2 in
  { Grid.blocked = mkblkd w h pr seed;
    Grid.costs = cost;
    Grid.moves = move;
    Grid.goal = [gx, gy];
    Grid.start = x0, y0;
    Grid.instance = Grid.not_known }

let load fname =
  Wrio.with_infile fname read

let write seed pr brd outch =
  let w = Grid.width brd and h = Grid.height brd in
  let move = brd.Grid.moves and cost = brd.Grid.costs in
  let gx, gy = List.hd brd.Grid.goal and x0, y0 = brd.Grid.start in
  fprintf outch "%d %d %d %g\n" seed w h pr;
  begin match move with
    | Grid.Fourway -> fprintf outch "Fourway\n"
    | Grid.Eightway -> fprintf outch "Eightway\n";
  end;
  begin match cost with
    | Grid.Unit -> fprintf outch "Unit\n"
    | Grid.Life -> fprintf outch "Life\n"
  end;
  fprintf outch "%d %d\n%d %d\n" gx gy x0 y0

let save seed pr brd path =
  Wrio.with_outfile path (write seed pr brd)

let rndinst w h pr move cost =
  let gx = w - 1 and gy = h - 1 in
  let x0 = 0 and y0 = 0 in
  let seed = Random.bits () in
  { Grid.blocked = mkblkd w h pr seed;
    Grid.costs = cost;
    Grid.moves = move;
    Grid.goal = [gx, gy];
    Grid.start = x0, y0;
    Grid.instance = Grid.not_known }, seed

let feasible move cost sol w h pr =
  let solvable (brd, seed) =
    let x0, y0 = brd.Grid.start in
    if brd.Grid.blocked.(x0).(y0) then
      false
    else
      sol { brd with Grid.costs = Grid.Unit } <> None in
  Wrutils.eval_until (fun () -> rndinst w h pr move cost) solvable

