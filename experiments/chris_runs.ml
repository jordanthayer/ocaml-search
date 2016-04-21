(*
Sliding_tiles_runs.do_batch 
  ~limits:[(Limit.Generated 100000000);(Limit.Time 300.)]
  "random"
  ~ncols:7 ~nrows:7 
  "bulb 5 6000000"
  [("alg","bulb");("beam_width","5");("node_capacity","6000000");];;
*)


type alg_package = 
{
  alg_name: string;
  arguments: (string * string) list;
};;

let convert_alg_package ap = 
  let args = snd (List.split ap.arguments) in
  let arg_str = Wrlist.sprint Fn.identity " " args in
    (ap.alg_name ^ " " ^ arg_str),
    ("alg",ap.alg_name)::ap.arguments;;


let make_bulb_package bw_list node_capacity = 
  List.map (fun a -> 
	      {
		alg_name = "bulb";
		arguments = [("beam_width",string_of_int a);
			     ("node_capacity",string_of_int node_capacity);];
	      }) bw_list;;


let make_sb_package bw_list node_capacity = 
  List.map (fun a -> 
	      {
		alg_name = "bounded_stochastic_beam";
		arguments = [("beam_width",string_of_int a);
			     ("node_capacity",string_of_int node_capacity);];
	      }) bw_list;;



let make_bounded_beam_stack_package bw_list node_capacity = 
  List.map (fun a -> 
	      {
		alg_name = "bounded_beam_stack_search";
		arguments = [("beam_width",string_of_int a);
			     ("node_capacity",string_of_int node_capacity);];
	      }) bw_list;;


let make_new_beam_package name bw_list order = 
  List.map (fun a -> 
	      {
		alg_name = name;
		arguments = [("beam_width",string_of_int a);
			     ("sort_predicate", order);];
	      }) bw_list;;


let make_bucketed_stratified_new_beam_package name bw_list order bucket = 
  List.map (fun a -> 
	      {
		alg_name = name;
		arguments = [("beam_width",string_of_int a);
			     ("sort_predicate", order);
			     ("bucket_size", string_of_float bucket)
			    ];
	      }) bw_list;;


let make_wted_beam_package 
    ?(bw_list = Experiments.full_beams)
    ?(wt_list = Experiments.low_res_weights)
    ?(order = "f")
    name = 
  List.map (fun wt -> List.map (fun a -> 
				  {
				    alg_name = name;
				    arguments = [("beam_width",string_of_int a);
						 ("sort_predicate", order);
						 ("wt",string_of_float wt);
						];
				  }) bw_list) wt_list;;
let make_wted_bf_beam_package 
    ?(bw_list = Experiments.full_beams)
    ?(wt_list = Experiments.low_res_weights)
    name = 
  List.map (fun wt -> List.map (fun a -> 
				  {
				    alg_name = name;
				    arguments = [("beam_width",string_of_int a);
						 ("wt",string_of_float wt);
						];
				  }) bw_list) wt_list;;


let make_wted_package name wt_list = 
  List.map (fun a -> 
	      {
		alg_name = name;
		arguments = [("wt", string_of_float a);];
	      }) wt_list;;


let make_beam_package name beam_list = 
  List.map (fun a -> 
	      {
		alg_name = name;
		arguments = [("beam_width", string_of_int a);];
	      }) beam_list;;


let make_ib_dbb_package bw_list node_capacity p_beam rsp alg_name = 
  assert (p_beam < 1.0);
  List.map (fun a -> 
	      let depth_bound = node_capacity / a in 
	      let bw_to_use = int_of_float ((float_of_int a)
					    *. p_beam) in
		{
		  alg_name = alg_name;
		  arguments = [("beam_width",string_of_int bw_to_use);
			       ("sort_predicate","f");
			       ("reserve_sort_predicate",rsp);
			       ("depth_bound",string_of_int depth_bound);
			       ("node_capacity",string_of_int node_capacity);
			      ];
		}) bw_list;;




let call_search 
    (do_batch: string -> (string * string) list -> unit) 
    (ap:alg_package) = 
  let args, alg_attrs = convert_alg_package ap in
    do_batch args alg_attrs


let sliding_tiles_7x7 = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);
	   (Limit.MachineMemory)]
  "random"
  ~ncols:7 ~nrows:7 ;;


let extended_sliding_tiles_7x7 = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 1000.);
	   (Limit.MachineMemory)]
  "random"
  ~ncols:7 ~nrows:7 ;;



let sliding_tiles_korf = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  "korf"
  ~ncols:4 ~nrows:4 ;;

let sliding_tiles_korf_inverse = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  ~cost:"inverse"
  "korf"
  ~ncols:4 ~nrows:4 ;;

let sliding_tiles_korf_sqrt = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  ~cost:"sqrt"
  "korf"
  ~ncols:4 ~nrows:4 ;;

let sliding_tiles_8 = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  "random"
  ~ncols:3 ~nrows:3 ;;



let sliding_tiles_11 = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  "random"
  ~ncols:4 ~nrows:3 ;;

let sliding_tiles_11_inverse = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  ~cost:"inverse"
  "random"
  ~ncols:4 ~nrows:3 ;;

let sliding_tiles_11_sqrt = Sliding_tiles_runs.do_batch 
  ~send_mail:false
  ~limits:[(Limit.Time 300.);(Limit.MachineMemory);]
  ~cost:"sqrt"
  "random"
  ~ncols:4 ~nrows:3 ;;



let ib_beams =
  [5;
   10;
   50;
   100;
   500;
   1000;
   5000;
   10000;]


