(* Does runs on the learning algs
   Jordan Thayer - June 09
*)

(*open Grid_runs*)

(* Some simplified algorithm calls *)

let board_to_dom w =
  match w.Grid.costs with
      Grid.Unit ->
	(match w.Grid.moves with
	   | Grid.Fourway -> Learned_wts.Unit4
	   | Grid.Eightway -> Learned_wts.Unit8)
    | Grid.Life ->
	(match w.Grid.moves with
	   | Grid.Fourway -> Learned_wts.Life4
	   | Grid.Eightway -> Learned_wts.Life8)

let exact_vector b =
  Learned_wts.get_exact_vect (Grid.width b) (Grid.height b) b.Grid.instance
    (board_to_dom b)


let avg_vect b =
  Learned_wts.get_avg_vect (Grid.width b) (Grid.height b) b.Grid.instance
    (board_to_dom b)


let get_bootstrapped_vector b =
  Learned_wts.get_bs_vect (Grid.width b) (Grid.height b) b.Grid.instance
    (board_to_dom b)

let exact_vector_wastar wt w lim =
  Grid_learning_algs.vector_wted_astar w lim
    (exact_vector w Learned_wts.M4) wt


let exact_vector_clampped_dd wt w lim =
  Grid_learning_algs.vector_clamped_dd w lim
    (exact_vector w Learned_wts.M4) wt


let exact_vector_fheps wt w lim =
  Grid_learning_algs.vector_fheps w lim
    (exact_vector w Learned_wts.M4) wt

let exact_vector_ltqs wt w lim =
  Grid_learning_algs.vector_ltqs w lim
    (exact_vector w Learned_wts.M4) wt


let boot_clampped_dd_m4 wt w lim =
  Grid_learning_algs.vector_clamped_dd w lim
    (get_bootstrapped_vector w Learned_wts.M4) wt


let boot_fheps_m4 wt w lim =
  Grid_learning_algs.vector_fheps w lim
    (get_bootstrapped_vector w Learned_wts.M4) wt


let boot_tqs_m4 wt w lim =
  Grid_learning_algs.vector_ltqs w lim
    (get_bootstrapped_vector w Learned_wts.M4) wt

let avg_vector_wastar wt w lim =
  Grid_learning_algs.vector_wted_astar w lim (avg_vect w Learned_wts.M4) wt


let avg_vector_wastar_dd wt w lim =
  Grid_learning_algs.vector_wted_astar_dd w lim (avg_vect w Learned_wts.M4) wt


let avg_clamped_m4 wt w lim =
  Grid_learning_algs.vector_clamped w lim (avg_vect w Learned_wts.M4) wt


let avg_clamped_m4_dd wt w lim =
  Grid_learning_algs.vector_clamped_dd w lim (avg_vect w Learned_wts.M4) wt


let avg_aseps_m4 wt w lim =
  Grid_learning_algs.vector_aseps w lim (avg_vect w Learned_wts.M4) wt

let avg_fheps_m4 wt w lim =
  Grid_learning_algs.vector_fheps w lim (avg_vect w Learned_wts.M4) wt


let t_fheps wt w lim =
  Grid_learning_algs.vector_fheps w lim (avg_vect w Learned_wts.M4) wt


(******************* Recording for the learning algs ************************)

let r_lfheps_exp wt w lim =
  let v = (Learned_wts.game_vector w Learned_wts.M4) in
  Grid_record.record_lfheps_exp wt w lim v v

let r_lfheps_open wt w lim =
  let v = (Learned_wts.game_vector w Learned_wts.M4) in
  Grid_record.record_lfheps_open wt w lim v v

let r_lfheps_focal wt w lim =
  let v = (Learned_wts.game_vector w Learned_wts.M4) in
  Grid_record.record_lfheps_focal wt w lim v v

let r_lfheps_clean wt w lim =
  let v = (Learned_wts.game_vector w Learned_wts.M4) in
  Grid_record.record_lfheps_clean wt w lim v v


let r_laseps_exp wt w lim =
  Grid_record.record_laseps_exp wt w lim (Learned_wts.game_vector w Learned_wts.M4)

let r_laseps_open wt w lim =
  Grid_record.record_laseps_open wt w lim (Learned_wts.game_vector w Learned_wts.M4)

let r_laseps_focal wt w lim =
  Grid_record.record_laseps_focal wt w lim (Learned_wts.game_vector w Learned_wts.M4)
(* EOF *)
