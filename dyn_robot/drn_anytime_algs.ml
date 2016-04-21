(* Anytime Algs for dynamic robots *)
open Drn_instance
open Dynamic

let hd problem =
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem) in
    (fun n -> h n, d n)

let mlogger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f i ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!"
	     f i.Limit.expanded i.Limit.generated t;
	   threshold := f *. 0.999)



let make_bgoal_p problem =
  let gx, gy, gh = problem.goal
  and delta = Math.round heading_delta in
    (fun s ->
       (s.speed = 0) &&
       (near s.x gx) &&
       (near s.y gy) &&
       ((h_diff s.heading gh) < delta))


let anytime_a_star wt problem l =
  let problem = expand_obstacles problem in
    Anytime_astar.anytime_wted_a_star_dups
      ~limit:l
      ~weight:wt
      (make_root problem)
      goal_p expand key (make_approx_time_h problem)
      (mlogger ())



let anytime_dwaredux wt problem l =
  let problem = expand_obstacles problem in
    Anytime_dwastar.search_dups
      ~limit:l
      ~weight:wt
      (make_root problem)
      goal_p expand key (hd problem)
      (mlogger ())


let rlogger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f e g ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!"
	     f e g t;
	   threshold := f *. 0.999)



let richterl1 problem l =
  let problem = expand_obstacles problem in
    Richter.richter_dups
      ~limit:l
      (make_root problem)
      goal_p
      expand
      key
      (make_approx_time_h problem)
      Richter.richterl1
      (rlogger ())


let richterl2 problem l =
  let problem = expand_obstacles problem in
    Richter.richter_dups
      ~limit:l
      (make_root problem)
      goal_p
      expand
      key
      (make_approx_time_h problem)
      Richter.richterl2
      (rlogger ())



let aralog () =
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f i _ ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!"
	     f i.Limit.expanded i.Limit.generated t;
	   threshold := f *. 0.999)



let ara_star wt problem l =
  let problem = expand_obstacles problem in
    Arastar2.ara_star
      ~wts:(Arastar2.mk_wtlist wt 0.2)
      ~limit:l
      (make_root problem)
      goal_p
      expand
      (make_approx_time_h problem)
      key
      (aralog ())



(*
let bugsy cost_coeff time_coeff expands_per problem =
  let problem = expand_obstacles problem in
  let s, e, g = Bugsy.search cost_coeff time_coeff
		  ~default_u:neg_infinity ~deadline:infinity
		  expands_per
		  (make_hd problem)
		  (* nearest = cheapest as far as the heuristic goes, so h
		     is enough *)
		  (Wrutils.constantly1 (1e10, 100000.))
		  (make_broot problem)
		  (make_bgoal_p problem)
		  (make_bexpand problem) in
    match s with
      None -> failwith "no solution?"
    | Some pair -> pair, e, g
*)


(* EOF *)
