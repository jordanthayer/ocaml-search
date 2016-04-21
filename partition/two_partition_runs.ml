(** Running experiments on 2-way number partitioning.

    @author eaburns
    @since 2009-12-31
*)

open Printf

let solver_binary = User_paths.bin_root ^ "two_partition_solver"
let instance_root = User_paths.instance_root ^ "partition"
let data_root = User_paths.data_root ^ "partition"

let call_solver args prob_path attrs outch =
  (** [call_solver args prob_path attrs outch] calls the solver,
      writes the datafile and returns the cost and time. *)
  Wrsys.with_subprocess_pipes
    (Wrutils.str "%s %s < %s\n" solver_binary args prob_path)
    (fun to_solver from_solver ->
       Datafile.write_header_pairs outch;
       Datafile.write_pairs outch attrs;
       Datafile.write_pairs outch ["attrs", Wrstr.encode_pairs attrs];
       let res = Datafile.pipe_data from_solver outch in
	 Datafile.write_trailer_pairs outch;
	 res)


let do_run overwrite limit_str args attrs inst_attrs =
  (** [do_run overwrite limit_str args attrs inst_attrs] runs the
      two-partition solver on the instance described by the
      [inst_attrs]. *)
  let prob_path = Rdb.path_for instance_root inst_attrs in
  let args = limit_str ^ " " ^ args in
  let attrs = ("ways", "2") :: attrs @ inst_attrs in
  let out_file = Rdb.path_for data_root attrs in
    if (Sys.file_exists out_file
	&& Datafile.seems_complete out_file
	&& not overwrite)
    then printf "%s exists - skipping!\n%!" out_file
    else
      Wrio.with_outfile out_file
	(fun outch ->
	   printf "Running %s on %s...%!"
	     args (Wrfname.make_relative instance_root prob_path);
	   let cost, time = call_solver args prob_path attrs outch in
	     printf "%.1f in %.3f.\n%!" cost time)


let do_batch ?(filter_attrs=(fun _ -> true))
    overwrite model limit_str args alg_attrs digits size =
  (** [do_batch overwrite limit_str args alg_attrs digits size] run
      the batch of instances of the given number of digits and
      size. *)
  let prob_attrs = [
    "model", model;
    "digits", string_of_int digits;
    "size", string_of_int size; ]
  in
  let inst_attrs =
    List.filter filter_attrs (Rdb.matching_attrs instance_root prob_attrs)
  in List.iter (do_run overwrite limit_str args alg_attrs) inst_attrs


let do_korf_batch overwrite limit_str args alg_attrs =
  (** [do_korf_batch overwrite model limit_str args alg_attrs] runs the
      algorithm on a batch of Korf-style instances. *)
  let digits = 10 in
    List.iter
      (do_batch overwrite "korf" limit_str args alg_attrs digits)
      (Lazy.force Two_partition_instance.korf_sizes)


let do_korf_batch_sparse overwrite limit_str args alg_attrs =
  (** [do_korf_batch overwrite limit_str args alg_attrs] runs the
      algorithm on a batch of Korf-style instances but only the first
      10 instances instead of the entire batch. *)
  let f attrs =
    let n = int_of_string (List.assoc "num" attrs) in
      n <= 50
  in
  let digits = 10 in
    List.iter
      (do_batch ~filter_attrs:f overwrite
	 "korf" limit_str args alg_attrs digits)
      (Lazy.force Two_partition_instance.korf_sizes)


let do_wheeler_batch overwrite limit_str args alg_attrs =
  (** [do_korf_batch overwrite model limit_str args alg_attrs] runs the
      algorithm on a batch of Korf-style instances. *)
  List.iter
    (fun (digits, size) ->
       do_batch overwrite "wheeler" limit_str args alg_attrs digits size)
    [
      15, 32;
      25, 64;
      44, 128;
      82, 256;
      159, 512;
    ]


let polynomial_models () =
  (** [polynomial_models ()] Build a list of the polynomial models *)
  let lst = ref [] in
    for degree = 20 to 20 do
      Wrutils.push
	(Wrutils.str "blfs_mixed nlms_flow_2 0.02 %d 4096" degree,
	 [
	   "alg", "blfs_mixed";
	   "learner", "nlms_flow_2";
	   "learning rate", "0.02";
	   "degree", string_of_int degree;
	   "samples", "4096";
	 ])
	lst;
      Wrutils.push
	(Wrutils.str "blfs_poly nlms_flow_2 0.02 %d" degree,
	 [
	   "alg", "blfs_poly";
	   "learner", "nlms_flow_2";
	   "learning rate", "0.02";
	   "degree", string_of_int degree
	 ])
	lst;
    done;
    !lst


let separate_models () =
  (** [separate_models ()] gets a list of the separate models. *)
  [
    Wrutils.str "blfs_sep nlms_flow_2 0.02",
    [
      "alg", "blfs_sep";
      "learner", "nlms_flow_2";
      "learning rate", "0.02";
    ];
    Wrutils.str "blfs_sep_sampled nlms_flow_2 0.02 4096",
    [
      "alg", "blfs_sep_sampled";
      "learner", "nlms_flow_2";
      "learning rate", "0.02";
      "samples", "4096";
    ];
  ]

let algs kind =
  (** [algs kind] gets a list of the algorithms with the given kind
      added to their args string and attributes. *)
(*
  (if kind = "ckk_list" || kind = "ckk" || kind = "greedy"
   then
     List.map
       (fun (s, attrs) ->
	  s ^ " -k " ^ kind, ("kind", kind)::attrs)
       ((polynomial_models ())
	@ (separate_models ()))
   else [])
  @
*)
    (if kind = "greedy"
     then [ Wrutils.str "indecision_sum -k %s" kind,
	    ["kind", kind; "alg", "indecision_sum"]
	  ]
     else [])
  @
    [
      Wrutils.str "dfs -k %s" kind,
      ["kind", kind; "alg", "dfs";];

      Wrutils.str "ilds_top -k %s" kind,
      ["kind", kind; "alg", "ilds_top";];

      Wrutils.str "ilds_bottom -k %s" kind,
      ["kind", kind; "alg", "ilds_bottom";];

      Wrutils.str "dds -k %s" kind,
      ["kind", kind; "alg", "dds";];
    ]


let kinds = ["ckk_list"; "greedy"; "ckk"; "ckk_inplace"; "greedy_inplace"]


let korfish_runs kind overwrite limit =
  (** [korfish_runs kind overwrite limit] runs all of the algorithms
      on some Korf-like instances. *)
  List.iter
    (fun (arg, attr) -> do_korf_batch_sparse overwrite limit arg attr)
    (algs kind)


let wheelerish_runs kind overwrite limit =
  (** [wheelerish_runs kind overwrite limit] runs all of the
      algorithms on some Wheeler-like instances. *)
  List.iter
    (fun (arg, attr) -> do_wheeler_batch overwrite limit arg attr)
    (algs kind)
