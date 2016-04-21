(**

    @author jtd7
    @since 2010-10-21
   Simulates features and learning down particular paths
*)
module RR = Recorded_run

type step =
    { target : float;
      features : float array; }


let count_examples path = List.length path - 1


let standard_features n =
  [| RR.get_h n; RR.get_d n; RR.get_g n; 1.|]


let step_list_to_arrays sl =
  let leng = List.length sl in
  let t_array = Array.create leng nan
  and f_array = Array.create leng [||] in
    Wrlist.iteri (fun i e -> t_array.(i) <- e.target;
		    f_array.(i) <- e.features)  sl;
    t_array, f_array


let apply_model ?(norm_cost = Fn.identity)
    ?(feature_fun = standard_features) run path =
  let rec help = function
    | []
    | [_] -> []
    | parent::child::tl ->
	(if ((RR.has_node run parent) &&
	       (RR.has_node run child))
	 then
	   (let p = RR.get_node run parent
	    and bc = RR.get_node run child in
	    let c = (RR.get_g bc) -. (RR.get_g p) in
	    let target = (RR.get_h bc) +. c in
	      {target = norm_cost target;
	       features = feature_fun p}::(help (child::tl)))
	 else help (child::tl)) in
    help path


let run_features_macro run =
  let paths = RR.all_paths run in
    step_list_to_arrays (List.flatten (List.map (apply_model run) paths))



(* EOF *)
