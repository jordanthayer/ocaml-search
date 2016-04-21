open Tpl_search_reg

let anytime_a_star wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Anytime_astar.anytime_wted_a_star ~limit:l ~weight:wt
      root goal_p expand h
      (make_logger())


let anytime_dyn wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p
  and _, _, _, hd = Tpl_regression.init_aseps d p in
  let depth = truncate (ceil (match (hd root) with _,d -> d)) in
    Anytime_dyn_astar.anytime_dyn_astar ~limit:l
      root
      goal_p
      expand
      h
      wt
      depth
      (make_logger())


let anytime_dwaredux wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Anytime_dwastar.search
      ~limit:l
      ~weight:wt
      root
      goal_p
      expand
      hd
      (make_logger())


let mrl () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f e g ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   (* cost, exp generated time *)
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!"
	     f e g t;
	   threshold := f *. 0.999)


let richterl1 d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Richter.richter
      ~limit:l
      root
      goal_p
      expand
      Tpl_regression.key
      h
      Richter.richterl1
      (mrl())


let richterl2 d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Richter.richter
      ~limit:l
      root
      goal_p
      expand
      Tpl_regression.key
      h
      Richter.richterl2
      (mrl())




let arastar wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p
  and log3 = make_logger () in
  let s, e, g, p, q, d =
    (Arastar2.ara_star
       ~wts:(Arastar2.mk_wtlist wt 0.2)
       ~limit:l
       root
       goal_p
       expand
       h
       Tpl_regression.key
       (fun a b c _ -> log3 a b c)) in
    s,e,g,p,q

(* EOF *)
