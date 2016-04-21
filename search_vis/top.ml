(** Top level for interacting with the search visualizer *)

let timing = ref 0.1

let busy_wait dur =
  if dur > 0. then ignore (Unix.select [] [] [] dur)

(*
let interactive_loop disp =
(** runs an interactive display loop on the visualizer *)
  let go = ref true in
    while(!go)
    do
      try Display.actionloop disp
      with Failure str -> (Verb.pe Verb.toplvl "%s\n%!" str; go := false)
    done
*)

(*
let d = Top.load_grid_run
  ~truth:".research/search_vis/recorded_runs/truth/dforest"
  ".research/search_vis/test_data/dforest_dfs.exp"
  [".research/search_vis/test_data/dforest_dfs.str"]
  ".research/search_vis/problems/dforest";;
*)

let load_grid_run ?(truth = "") exp str_list problem =
  (** Returns a display object representing the gridp problem
      [truth] a file containing the true heuristic values of all nodes
      [exp] a file containing all nodes expanded by the search and related data
      [str_list] contains a list of files representing data structures used
      [problem]  The instance description *)
  Node_display.node_window#show();
  Dpq_display.dpq_window#show();
  Grid_display.grid_window#show();
  Run.run_window#show();
  Verb.pe Verb.always "Windows shown\n";
  let d = Display.setup_grid
    (Run.load_grid_run ~tr_file:truth exp str_list problem) in
    ignore (Grid_display.grid_window#event#connect#any
	      ~callback:(fun _ -> Display.redraw d; true));
    (*above forces repaint whenever I do anything interesting to the
      grid display *)
    Display.redraw d;
    d


(** Note that the arbitrary moves are doing the redraws, not just resetting
    frame and redrawing **)
let step_forward disp n =
  (** Moves forward [n] steps *)
  for i = 0 to (n - 1)
  do
    (Display.stepForward disp;
     busy_wait !timing)
  done;
  Display.redraw disp

let step_backward disp n =
  (** Moves backward [n] steps *)
  for i = 0 to (n - 1)
  do
    (Display.stepBackward disp;
     busy_wait !timing)
  done;
  Display.redraw disp


let goto_step disp n =
  (** Moves to an arbitrary step [n]*)
  let delta = n - disp.Display.frame in
    if delta > 0
    then step_forward disp delta
    else step_backward disp (-delta)


let next disp =
  (** single forward step *)
  Display.stepForward disp


let prev disp =
  (** single backward step *)
  Display.stepBackward disp


let cycle_context disp =
  (** Swap what attribute is currently being displayed*)
  Display.cycle_context disp


let set_context disp str =
  Display.set_context disp str


let set_step disp t =
  (** sets the current runstep of the display to [t] and redraws it. *)
  assert (t >= 0);
  disp.Display.frame <- t;
  if disp.Display.frame >= (disp.Display.run.Run.run_leng - 1)
  then disp.Display.frame <-(disp.Display.run.Run.run_leng - 1);
  Display.redraw disp


let step_forward_by disp st n =
  for i = 0 to (n / st)
  do
    (set_step disp (disp.Display.frame + st);
     busy_wait !timing;)
  done;
  Display.redraw disp


let set_step_forward disp n =
  (** Moves forward [n] steps w/o redraw*)
  set_step disp (disp.Display.frame + n)


let move_forward_by_struct disp n =
  (** Moves forward by the [n]th display *)
  let strct = List.nth disp.Display.run.Run.run.Recorded_run.structs n in
  let sequence = strct.Structure.sequence in
    List.iter (fun step -> busy_wait !timing; set_step disp step) sequence


let resize_grid disp sz =
  Constants.grid_squareSize := sz;
  Display.redraw disp


let reset_timing v =
  timing := v


let time_to_sol disp duration =
  (** Makes sure you can finish the rest of the current run in [duration]
      minutes from the current state *)
  let steps = float_of_int (disp.Display.run.Run.run_leng - disp.Display.frame)
  and duration = duration *. 60.
  in timing := duration /. steps

(* EOF *)
