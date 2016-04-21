(**

    @author jordan
    @since 2011-06-21

   improved version of lmix that scores also on f
*)

open Lmix_search

let make_expand expand hd_cheap d_near =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  let lmix_expand alpha n =
    assert (alpha >=0.);
    assert (alpha <= 1.);
    let beta = 1. -. alpha in
    let depth' = n.fpv.depth +. 1. in
    List.map (fun (data, g) ->
      let hc,dc = hd_cheap data in
      let dn = d_near data in
      let f = hc +. g
      and ln = depth' +. dn
      and lc = depth' +. dc in
      let score = (alpha *. ln +. beta *. lc) *. alpha +. beta *. f in
      let fpv = { depth = depth'; g = g; f = f;
		  score = score; } in
      { data = data;
	pos = Dpq.no_position;
	fpv = fpv;
	kind = Both }) (expand n.data n.fpv.g) in lmix_expand


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let module H = Heuristic in
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and goal = wrap sface.SI.goal_p
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and root = { data = sface.SI.initial;
	       fpv = { depth = 0.; g = 0.; f = 0.; score = 0.;};
	       kind = Both;
	       pos = Dpq.no_position; }
  and hd_cheap = (H.default_hd sface.SI.heuristics).H.heuristic
  and d_near = H.get_fixed sface.SI.heuristics [H.Nearest; H.Distance] in
  let d_near = (List.hd d_near).H.heuristic in
  let expand = make_expand sface.SI.domain_expand hd_cheap d_near
  and def_log = (Limit.make_default_logger (fun n -> n.fpv.f)
		   (fun n -> truncate n.fpv.depth)) in
  let info = Limit.make Limit.Nothing sface.SI.halt_on just_f def_log in
  search expand key goal hash equals root info;
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)

