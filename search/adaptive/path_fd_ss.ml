(**

   @author jtd7
   @since 2010-12-23

   Path based corrections for use in EES
*)

let epsilon = 0.0001

let make_unbounded_correction_oldschool get_g get_h get_d get_depth get_herr
    get_derr =

  let h_calc parent best children =
    ()
  and fd_calc n =
    let fdepth = float (get_depth n) in
    let herr = (get_herr n) /. fdepth
    and derr = (get_derr n) /. fdepth in
    let nd = (1. +. derr) *. (get_d n) in
    let nf = (get_g n) +. (get_h n) +. (herr *. nd) in
(*      assert (Math.finite_p nd);
      assert (Math.finite_p nf);*)
      nf, nd in
    h_calc, fd_calc

let make_unbounded_correction_austin get_g get_h get_d get_depth get_herr
    get_derr =

  let h_calc parent best children =
    ()

  and fd_calc n =
    let fdepth = float (get_depth n) in
    let herr = (get_herr n) /. fdepth
    and derr = (get_derr n) /. fdepth in
    let based = get_d n
    and baseh = get_h n in
    let nd = Math.fmax based (if derr >= 1.
			      then based /. epsilon
			      else based /. (1. -. derr)) in
    let nf = (get_g n) +. (Math.fmax baseh (baseh +. (herr *. nd))) in
      (*Verb.pe Verb.debug "%f -> %f\t %f -> %f\n%!"
	(baseh +. (get_g n)) nf based nd;*)
      (*      assert (Math.finite_p nd);
	      assert (Math.finite_p nf);
	      assert (nd >= 0.);
	      assert (nf >= 0.); *)
      nf,nd
  in
    h_calc, fd_calc


let make_unbounded_correction_austin_hcalc get_h get_d get_depth get_herr
    get_derr =
  (fun n ->
    let fdepth = float (get_depth n) in
    let herr = (get_herr n) /. fdepth
    and derr = (get_derr n) /. fdepth in
    let based = get_d n
    and baseh = get_h n in
    let nd = Math.fmax based (if derr >= 1.
			      then based /. epsilon
			      else based /. (1. -. derr)) in
    let nf = (Math.fmax baseh (baseh +. (herr *. nd))) in
      nf)
(* EOF *)
