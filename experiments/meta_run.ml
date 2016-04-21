(** Meta_run.ml -  Runs experiments on algorithms across all domains.
    Whenever a new domain is added, meta_run will need to be updated so that
    algorithms are checked against the new domain as well.
    Jordan - August 2009 *)

let drops_duplicates string =
  false
  (*Str.string_match (Str.regexp "dd") (Str.last_chars string 2) 0*)


let do_basic_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit)) ?(overwrite = false)
    alg =
  Notify.start_metatime();
(*  Msa_runs.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;*)
  Tiles_runs.do_basic_batch ~models:["korf"]
    ~time_limit:time_limit ~node_limit:node_limit alg;
(*  Tpl_runs_reg.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;*)
  Grid_runs.do_basic_batch
    ~overwrite:overwrite ~time_limit:time_limit ~node_limit:node_limit alg;
  Drn_runs.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;
  Vacuum_runs.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;
  Pancake_runs.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;
  Tsp_runs.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;
(*;  Synth_runs.do_basic_batch
    ~time_limit:time_limit ~node_limit:node_limit alg;
  Rucksack_runs.do_basic_batch
    ~node_limit:node_limit ~time_limit:time_limit alg*)
  Notify.send_metarun_completed_mail "Basic" alg


let do_wted_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) ?(overwrite = false) alg =
  Notify.start_metatime();
  (*  Msa_runs.do_wted_batches
      ~time_limit:time_limit ~node_limit:node_limit  ~weights:weights alg;*)
  Tiles_runs.do_wted_batch ~models:["korf"] ~overwrite:overwrite
    ~time_limit:time_limit ~node_limit:node_limit  ~weights:weights alg;
  Grid_runs.do_wted_batches ~time_limit:time_limit ~node_limit:node_limit
    ~weights:weights ~overwrite:overwrite alg;
  Drn_runs.do_wted_batches ~overwrite:overwrite
    ~time_limit:time_limit ~node_limit:node_limit  ~weights:weights alg;
  if not (drops_duplicates alg)
  then ((*Tpl_runs_reg.do_wted_batch
	  ~time_limit:time_limit ~node_limit:node_limit  ~weights:weights alg;*)
	Vacuum_runs.do_wted_batches ~overwrite:overwrite
	  ~time_limit:time_limit ~node_limit:node_limit alg;
	Pancake_runs.do_wted_batches ~overwrite:overwrite
	  ~time_limit:time_limit ~node_limit:node_limit alg;
	Tsp_runs.do_wted_batches ~overwrite:overwrite
	  ~time_limit:time_limit ~node_limit:node_limit  ~weights:weights alg(*;
	Synth_runs.do_wted_batches ~overwrite:overwrite
	  ~time_limit:time_limit ~node_limit:node_limit alg;
	Rucksack_runs.do_wted_batches
	  ~node_limit:node_limit ~time_limit:time_limit alg*));
  Notify.send_metarun_completed_mail "Weighted" alg


let do_beam_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) ?(overwrite = false) alg =
  Notify.start_metatime();
(*  Msa_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit
    ~beam_widths:beam_widths alg;*)
  Tiles_runs.do_beam_batch
    ~time_limit:time_limit ~node_limit:node_limit ~beam_widths:beam_widths alg;
  Grid_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit ~beam_widths:beam_widths
    ~overwrite:overwrite alg;
  Drn_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit ~beam_widths:beam_widths alg;
  Tpl_runs_reg.do_beam_batch
    ~time_limit:time_limit ~node_limit:node_limit ~beam_widths:beam_widths alg;
  Tsp_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit ~beam_widths:beam_widths alg;
  Vacuum_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit alg;
  Pancake_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit alg;(*;
  Synth_runs.do_beam_batches
    ~time_limit:time_limit ~node_limit:node_limit alg;
  Rucksack_runs.do_beam_batches
    ~node_limit:node_limit ~time_limit:time_limit alg*)
  Notify.send_metarun_completed_mail "Beam" alg


let do_optimistic_batch
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(optimisms = Experiments.optimisms) ?(overwrite = false) alg =
  Notify.start_metatime();
  Grid_runs.do_optimistic_batches ~time_limit:time_limit ~node_limit:node_limit
    ~weights:weights ~opt:optimisms ~overwrite:overwrite alg;
  (*  Msa_runs.do_optimistic_batches ~time_limit:time_limit ~node_limit:node_limit
      ~weights:weights ~opt:optimisms alg;*)
  Drn_runs.do_optimistic_batches ~time_limit:time_limit ~node_limit:node_limit
    ~weights:weights ~opt:optimisms alg;
  Tiles_runs.do_optimistic_batches ~time_limit:time_limit ~node_limit:node_limit
    ~weights:weights ~opt:optimisms alg;

  (*Synth_runs.do_optimistic_batches
    ~time_limit:time_limit ~node_limit:node_limit alg;*)
  if not (drops_duplicates alg)
  then ((*Tpl_runs_reg.do_optimistic_batches ~time_limit:time_limit ~node_limit:node_limit
	  ~weights:weights ~opt:optimisms alg;*)
	Tsp_runs.do_optimistic_batches ~time_limit:time_limit ~node_limit:node_limit
	  ~weights:weights ~opt:optimisms alg;
	Vacuum_runs.do_optimistic_batches
	  ~time_limit:time_limit ~node_limit:node_limit alg;
	Pancake_runs.do_optimistic_batches
	  ~time_limit:time_limit ~node_limit:node_limit alg
	  (*;
	Rucksack_runs.do_optimistic_batches
	  ~node_limit:node_limit ~time_limit:time_limit alg*));
  Notify.send_metarun_completed_mail "Optimistic" alg


(* EOF *)
