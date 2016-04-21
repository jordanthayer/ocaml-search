(**

    @author jtd7
    @since 2012-09-30
   Bootstrapping on top of greedy search
*)

type 'a node = {
  data : 'a;
  g : float;
  f : float;
  h : float;
  d : float;
  depth : int;
  mutable pos : int;
  parent : 'a node;
}

let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let h_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.h : float) < b.h) ||
    ((a.h = b.h) && (a.g < b.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let do_calc wts features =
  (** Calculates the scalar that is a result of applying the weight vector to
      the feature vector. *)
  fst
    (Array.fold_left
       (fun (product,index) wt ->
	  (product +. wt *. features.(index), index + 1)) (0.,0) wts)


let make_default_features max_h max_d _ g ch cd depth pg ph =
  let pf = pg +. ph
  and cf = g +. ch in
    [|ch /. max_h;
      g /. max_h;
      (g -. pg) /. max_h;
      (pf -. cf) /. max_h;
      (float depth) /. max_d|]

let default_feature_count = 5


let make_expand expand hd get_features getf max_h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (dat, g) ->
		 let depth' = n.depth + 1 in
		 let h,d = hd dat in
		 let f = (getf (get_features dat g h d depth' n.g n.h)) *.
		   max_h in
		   (* This was a sanity check to see learning happen *)
(*		   if not (Math.within f (g +. h) 0.05)
		   then Verb.pe Verb.always "%f %f %f \n%!" f g h;*)
		 { data = dat;
		   f = f;
		   g = g;
		   h = h;
		   d = d;
		   depth = depth';
		   pos = Dpq.no_position;
		   parent = n; }) (expand n.data n.g))


let batched_lms features targets  =
  let tar = Array.of_list targets
  and far = Array.of_list features in
  let new_weights = Offline_lms.lms far tar in
    (do_calc new_weights)


let ann num_features =
  let network = Ann.two_layer num_features 3 in
    (fun features targets ->
       let tar = Array.of_list targets
       and far = Array.of_list features in
	 Ann.batch_train_twolayer network far tar;
	 (*Ann.print_weights network;*)
	 Ann.two_layer_output network)


let train getf get_features learner (solution,target) =
  let max_h = solution.g
  and max_d = float solution.depth in
  let get_features = get_features max_h max_d in
  let target = target /. max_h in
  let rec build_features (targets, features) node =
    if node.parent == node
    then (targets, features)
    else (build_features
	    (target::targets,
	     ((get_features node.data node.g node.h node.d node.depth
		 node.parent.g node.parent.h)::features))
	    node.parent) in
  let targets, features = build_features ([],[]) solution in
    learner features targets


let make_sface ?(learner = batched_lms) base_sface feature_count get_features =
  let module SI = Search_interface in
  let getf = ref (fun far -> far.(0) +. far.(1)) in
  let def_log = Bootstrapping.make_logger (fun n -> n.g) (fun n -> n.depth) in
  let info = Limit.make Limit.Nothing [] just_f (fun _ -> ()) in
    (fun (path:string) inst (halt_on : Limit.t list) ->
       let inum = List.hd (List.rev (Str.split (Str.regexp "/") path)) in
       let inum = int_of_string inum in
       let def_log = def_log inum in
       let sface = base_sface inst halt_on in
       let h,d = sface.SI.hd sface.SI.initial in
       let rec init = {data = sface.SI.initial;
		       f = !getf (get_features h d sface.SI.initial 0.
				    h d 0 0. h);
		       g = 0.;
		       depth = 0;
		       pos = Dpq.no_position;
		       h = h;
		       d = d;
		       parent = init;} in
       let get_features = get_features init.h init.d in
	 info.Limit.halt_p <- Limit.make_halt_p halt_on;
	 info.Limit.log <- def_log;
	 info.Limit.incumbent <- Limit.Nothing;
	 info.Limit.start_time <- Sys.time();
	 SI.make
	   ~node_expand:(make_expand sface.SI.domain_expand
			   sface.SI.hd
			   get_features !getf init.h)
	   ~goal_p:(wrap sface.SI.goal_p)
	   ~key:(wrap sface.SI.key)
	   ~hash:sface.SI.hash
	   ~equals:sface.SI.equals
	   ~halt_on
	   ~info
	   sface.SI.domain
	   init
	   just_f
	   (fun i ->
	      sface.SI.info.Limit.log
		(Limit.unwrap_info (fun n -> n.data) i);
	      def_log i)),
  (fun solution -> getf := train !getf get_features learner solution),
  info


let dups_functions ?(feature_count = default_feature_count)
    ?(get_features = make_default_features) base_sface =
  let learner = ann feature_count in
  let make_sface, train, i = (make_sface ~learner base_sface feature_count
				get_features)
  and solver sface =
    let sol,_,_,_,_,_ =
      Best_first.search_dups sface h_then_g just_f setpos getpos in
      match sol with
	| Limit.Nothing -> None
	| Limit.Incumbent (q,n) -> Some (n,n.g)
  in make_sface, train, solver, i


let dd_functions ?(feature_count = default_feature_count)
    ?(get_features = make_default_features) base_sface =
  let learner = ann feature_count in
  let make_sface, train, i = (make_sface ~learner base_sface feature_count
				get_features)
  and solver sface =
    let sol,_,_,_,_,_ =
      Best_first.search_drop_dups sface h_then_g just_f setpos getpos in
      match sol with
	| Limit.Nothing -> None
	| Limit.Incumbent (q,n) -> Some (n,n.g)
  in make_sface, train, solver, i


let do_dups ~make_instance ~load_instance ~base_sface ~domain ~args =
  let iroot,attrs = Bootstrapping.domain_to_iroot_attrs domain
  and ins_min = Search_args.get_int "Greedy_bootstrap" args 0
  and tmin = Search_args.get_float "Greedy_bootstrap" args 1
  and tmax = Search_args.get_float "Greedy_bootstrap" args 2
  and make_sface, train, solver, i = dups_functions base_sface in
    (Bootstrapping.bootstrap ~ins_min ~tmax
       make_sface
       train
       solver
       make_instance
       load_instance
       iroot
       attrs
       tmin);
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let do_dd ~make_instance ~load_instance ~base_sface ~domain ~args =
  let iroot,attrs = Bootstrapping.domain_to_iroot_attrs domain
  and ins_min = Search_args.get_int "Greedy_bootstrap" args 0
  and tmin = Search_args.get_float "Greedy_bootstrap" args 1
  and tmax = Search_args.get_float "Greedy_bootstrap" args 2
  and make_sface, train, solver, i = dd_functions base_sface in
  Verb.pe Verb.always "Starting boot strap run %i %f %f\n%!" ins_min tmin tmax;
    (Bootstrapping.bootstrap ~ins_min ~tmax
       make_sface
       train
       solver
       make_instance
       load_instance
       iroot
       attrs
       tmin);
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)



(* EOF *)
