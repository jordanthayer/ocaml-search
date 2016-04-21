(** Single step error learned via streaming ANN corrections *)

let get_ann_string str =
  match str with
    | "vacuum" ->
"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003\134'mH[p\223\191tQ{#\232\129\234?Z\205M#\131\209\221?(identity\160\160\014\004\149\250I:7F\000\192o\210\156\226\146\028\000\192r\0303\170\231\005\t@\244[\128D\241'\184\191'sigmoid\160\160\014\004\002\214\142\168P\221\005@\200\007\186\231\197s\005@\176%\187\175\172>\244?{\015\005iB*\232\191\004\004\160\160\014\004n\182I\242\132y\014@\220\198\183\207D\159\014@\254\252\001\129J\150\n@\174\029\000t\006L\029\192\004\007@"

    | "tiles" ->
"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003W\\\158\207\195\030\227?u\027\133\165}\026\004\192Y\141|c\217\237\241?(identity\160\160\014\004\147\029\133;\184@\233?\245\011\220uo\233\235?\236\255\145<_!\015\192*\029q\028\213\169\239?'sigmoid\160\160\014\004\135\2089\219\147G \192\213\165W+\224B \192\144\173\2161\223\159\001\192\004N\001!\152\167\211\191\004\004\160\160\014\004 -\161W\176\167\241?T\216\242{\144\006\241?\020\021 \005\141\253\005@\221\174\159.Xv\b\192\004\007@"

    | "l4grid" ->
	"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003txfl}{\253?\127\192\227\152\186\245\000\192\169\r\162\026\027\198\000@(identity\160\160\014\004\198<\171`\019\228\248\191\200W\187\128\154\005\236?\t\234\132\249\167{\006@\004\177\t\194\142\003\t\192'sigmoid\160\160\014\004\213>\023\005%\014\190\191\239\238p\180\bJ\211?4\151\174\156\001V\007@\225C\242:\003[\232?\004\004\160\160\014\004\143$T\220\196\028\030@\253\255\249x\024p\219?~\255\000\234]\015\250?\225\159\029\157V\230\211?\004\007@"

    | "u4grid" ->
"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003\199@\203\228\235:\004@\173\014\134\026\245\182\250\191y\016KMP\031\254?(identity\160\160\014\004\157b\001\139*.\244?\012,c[\143\136\231?\128]Ap\2311\014@;\170\016\r$$\006\192'sigmoid\160\160\014\004\225\025K\031x|\224?`\002\244$\017\135\177?\164\148\146@oB\018@\243\143\133\176.\019\002\192\004\004\160\160\014\004\027\147\128\"\148\158\201?\255\129\159\194\0063\192?\161\022\1777\169\017\005\192\251v\131\030\221S\241\191\004\007@"
    | _ -> failwith "Not found"


let make_greedy_correction max_features est_scale get_features get_g get_h =
  let hat_net = Ann.two_layer (Array.length max_features) 3 in
  let norm n = Wrarray.map2 (/.) (get_features n) max_features in
  let show_ex n t = Ann.show_example_twolayer hat_net (norm n) (t /. est_scale)
  and est n = (Ann.two_layer_output hat_net (get_features n) *. est_scale) in


  let h_calc parent best children =
    let target = (get_h best) +. ((get_g best)  -. (get_g parent)) in
      if Math.finite_p target then
	ignore (show_ex parent target) in

  let f_calc n =
    est n in

    h_calc, f_calc


let make_rev_greedy_correction max_features est_scale get_features get_g get_h =
  let hat_net = Ann.two_layer (Array.length max_features) 3 in
  let norm n = Wrarray.map2 (/.) (get_features n) max_features in
  let show_ex n t = Ann.show_example_twolayer hat_net (norm n) (t /. est_scale)
  and est n = (Ann.two_layer_output hat_net (get_features n) *. est_scale) in
  let h_calc parent best children =
    let target = get_g best in
      if Math.finite_p target then ignore (show_ex parent target) in
  let f_calc n = est n in

    h_calc, f_calc



let make_fixed_greedy_correction max_features est_scale get_features get_g get_h
    network_string =
  let hat_net = (match (Ann.ann_from_string
			  (get_ann_string network_string))
		 with Ann.Twolayer t -> t | _ -> failwith "bad string") in
  let norm n = Wrarray.map2 (/.) (get_features n) max_features in
  let est n = (Ann.two_layer_output hat_net (norm n)) *. est_scale in


  let h_calc parent best children =
    ()

  and f_calc n =
    est n in

    h_calc, f_calc



let make_batch_delay_greedy_correction wait_length
    max_features est_scale get_features get_g get_h =
  let hat_net = Ann.two_layer (Array.length max_features) 3 in
  let norm n = Wrarray.map2 (/.) (get_features n) max_features in
  let show_ex n t = Ann.show_example_twolayer hat_net (norm n) (t /. est_scale)
  and est n = (Ann.two_layer_output hat_net (norm n) *. est_scale) in

  let batch_features = Array.create wait_length [||]
  and batch_targets = Array.create wait_length nan
  and counter = ref 0 in

  let h_calc parent best children =
    let target = (get_h best) +. ((get_g best)  -. (get_g parent)) in
      if !counter < wait_length
      then (batch_features.(!counter) <- norm parent;
	    batch_targets.(!counter) <- target /. est_scale;
	    counter := !counter + 1;
	    if !counter = wait_length
	    then (
	      Ann.batch_train_twolayer hat_net batch_features batch_targets))
      else ignore (show_ex parent target) in

  let f_calc n =
    if !counter < wait_length
    then get_h n
    else est n in
    h_calc, f_calc



let make_batch_delay_fhp_correction wait_length bound
    max_features est_scale get_features get_g get_h =
  let hat_net = Ann.two_layer (Array.length max_features) 3 in
  let norm n = Wrarray.map2 (/.) (get_features n) max_features in
  let show_ex n t = Ann.show_example_twolayer hat_net (norm n) (t /. est_scale)
  and est n = (Ann.two_layer_output hat_net (norm n) *. est_scale) in

  let batch_features = Array.create wait_length [||]
  and batch_targets = Array.create wait_length nan
  and counter = ref 0 in

  let h_calc parent best children =
    let target = (get_h best) +. ((get_g best)  -. (get_g parent)) in
      if !counter < wait_length
      then (batch_features.(!counter) <- norm parent;
	    batch_targets.(!counter) <- target /. est_scale;
	    counter := !counter + 1;
	    if !counter = wait_length
	    then (
	      Ann.batch_train_twolayer hat_net batch_features batch_targets))
      else ignore (show_ex parent target) in

  let f_calc n =
    if !counter < wait_length
    then (get_g n) +. (get_h n) *. bound
    else
      (let g = get_g n in
       let max = (g +. (get_h n)) *. bound in
       let est_f = g +. (est n) *. bound in
	 Math.fmin max est_f) in

    h_calc, f_calc

(* EOF *)
