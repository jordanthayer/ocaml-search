(* $Id: datafile.ml,v 1.2 2008/01/11 01:01:01 ruml Exp ruml $

   data files
*)


type t = {
  name : string;
  vals : (string, string) Hashtbl.t;
  cols : (string, float array) Hashtbl.t;
}

let dummy ?(non_path_vals = ["found solution", "no";
			     "final sol cost", "infinity"]) name rdb_attrs =
  let cols = Hashtbl.create 0
  and vals = Hashtbl.create (List.length rdb_attrs) in
    List.iter (fun (key,vl) -> Hashtbl.add vals key vl) rdb_attrs;
    List.iter (fun (key,vl) -> Hashtbl.add vals key vl) non_path_vals;
    { name = name;
      vals = vals;
      cols = cols;}


(******** using ***********)

let get_altcol df alt_col_name =
  try
    Hashtbl.find df.cols alt_col_name
  with Not_found ->
    failwith (Wrutils.str "%s not a valid column name" alt_col_name)


(* makes a list of the keys. *)
let get_keys df = Wrht.keys df.vals

let add_val df key value =
  Hashtbl.add df.vals key value


let get_name t = t.name


let has_val df key =
  match key with
    | "total cpu time"
    | "precise cpu time" -> true
    | _ ->
	(try
	   Hashtbl.mem df.vals key
	 with Not_found ->
	   failwith (Wrutils.str "has_val: %s not found" key))


let rec get_val df key =
  match key with
    | "total cpu time" ->
	(let d = get_val df "total raw cpu time"
	 and m = get_val df "machine id" in
	 let ar = Data.normalize_times m [| (float_of_string d)|] in
	   Wrutils.str "%f" ar.(0))
    | "precise cpu time" ->
	(let start = float_of_string (get_val df "wall start time")
	 and stop = float_of_string (get_val df "wall finish time") in
	   Printf.sprintf "%f" (stop -. start))
    | _ ->
	(try
	   Hashtbl.find df.vals key
	 with Not_found -> failwith (Wrutils.str "can't find val key \'%s\' in datafile.  valid val keys are: %s" key (String.concat ", " (Wrht.keys df.vals))))


let get_all_val df key =
  match key with
      "total cpu time" ->
	(let d = get_val df "total raw cpu time"
	 and m = get_val df "machine id" in
	 let ar = Data.normalize_times m [| (float_of_string d)|] in
	   [Wrutils.str "%f" ar.(0)])
    | _ ->
	(try
	   Hashtbl.find_all df.vals key
	 with Not_found -> failwith (Wrutils.str "can't find val key \'%s\' in datafile.  valid val keys are: %s" key (String.concat ", " (Wrht.keys df.vals))))


let print_df df =
  print_string "\n";
  List.iter (fun f ->
	       (Printf.fprintf stdout "%s : %s\n" f
		  (get_val df f))) (get_keys df);
  print_string "\n"



let get_raw_col df key =
  Verb.pe Verb.debug "Fetching key %s %s\n%!" df.name key;
  try
    Hashtbl.find df.cols key
  with Not_found ->
    Verb.pe Verb.debug "Can't find key %s\n" key;
    let msg = "can't find col key \'" ^ key ^
	      "\' in datafile [" ^ df.name ^ "].  valid col keys are: " ^
	      (String.concat ", " (Wrht.keys df.cols)) in
      failwith msg


let rec get_col data key =
  match key with
    "cpu time" ->
      let d = get_raw_col data "raw cpu time"
      and m = get_val data "machine id" in
	Data.normalize_times m d
    | "log10 cpu time" ->
	Array.map (Math.safe_log10 ~bound:(-9.)) (get_col data "cpu time")
    | "log10 nodes" -> Array.map log10 (get_raw_col data "nodes")
    | _ -> (let v = get_raw_col data key in
	      Verb.pe Verb.debug "Got raw col\n%!"; v)

let has_col df key =
  try
    Hashtbl.mem df.cols key
  with Not_found ->
    failwith (Wrutils.str "has_col: %s not found" key)



let clamped_col df key =
  let results = get_col df "best cost"
  and data = get_col df key
  and lowest = ref None in
    Wrarray.iter2 (fun r d ->
		     if r = -1. then
		       match !lowest with
			 None -> lowest := Some d
		       | Some m -> if d < m then lowest := Some d)
      results data;
    match !lowest with
      None -> data
    | Some clamp -> Array.map (min clamp) data


(********************** parsing **********************)


(******* format-agnostic utilities ******)


let stream_after_prefix prefix line =
  (** given string, returns stream that starts after prefix *)
  let strm = Stream.of_string line in
    Wrutils.ntimes (fun () -> Stream.junk strm)
      (String.length prefix);
    strm


let parse_val ht prefix line =
  (** adds to [ht] *)
  let strm = stream_after_prefix prefix line in
  let key = Wrstrm.parse_string strm
  and value = Wrstrm.parse_string strm in
    Hashtbl.add ht key value


let parse_col_names prefix line =
  (** returns list of (name, queue) pairs *)
  let strm = stream_after_prefix prefix line in
    List.map (fun n ->
		n, (Queue.create ()))
      (Wrstrm.parse_strings strm)


let parse_row cols line =
  (** given list of (name, queue) pairs and string, parses floats from
      string and adds them to the corresponding queues *)
  Verb.pe Verb.debug "Parse row\n%!";
  let nums = Wrstr.parse_floats line in
    if not ((List.length cols) = (List.length nums)) then
      failwith "wrong number of numbers in a row";
    List.iter2 (fun x (_, q) ->
		  Queue.add x q)
      nums cols


let from_cols cols =
  (** given list of (name, queue) pairs, returns hashtable *)
  let ht = Hashtbl.create (List.length cols) in
    List.iter (fun (n, q) ->
		 assert (not (Hashtbl.mem ht n));
		 Hashtbl.add ht n (Wrarray.of_queue q))
      cols;
    ht


(******* parsing format 3 ******)


let val_prefix_3 = "#Mdf3val "
let cols_prefix_3 = "#Mdf3cols "


let parse_line_3 vals cols line =
  if Wrstr.starts_as val_prefix_3 line then
    parse_val vals val_prefix_3 line
  else if Wrstr.starts_as cols_prefix_3 line then
    (match !cols with
	 [] -> cols := parse_col_names cols_prefix_3 line
       | _ -> failwith "already got col names")
  else if Wrstr.starts_with '#' line then
    failwith (Wrutils.str "strange line in Mdf3 file: %s\n" line)
  else
    (match !cols with
	 [] -> failwith "cols without names!"
       | _ -> parse_row !cols line)


let read_3 name line ch =
  let vals = Hashtbl.create 16
    (* list of (string, Queue.t) *)
  and cols = ref [] in
    parse_line_3 vals cols line;
    (try
       while true do
	 parse_line_3 vals cols (input_line ch)
       done
     with End_of_file -> ());
    { name = name;
      vals = vals;
      cols = from_cols !cols; }


(******* parsing format 4 *******)


let start_4 = "#start data file format 4"
let pair_prefix_4 = "#pair "
let cols_prefix_4 = "#cols "
let alt_cols_prefix_4 = "#altcols "
let alt_row_prefix_4 = "#altrow "
let end_4 = "#end data file format 4"


let parse_val_4 ht line =
  Verb.pe Verb.debug "parse val4\n%!";
  parse_val ht pair_prefix_4 line


let parse_col_names_4 line =
  Verb.pe Verb.debug "parse col names v4\n%!";
  parse_col_names cols_prefix_4 line


let parse_alt_col_names_4 alts line =
  Verb.pe Verb.debug "parse altcol names v4\n%!";
  let strm = stream_after_prefix alt_cols_prefix_4 line in
  let key = Wrstrm.parse_string strm in
    if (Hashtbl.mem alts key) then
      failwith (Wrutils.str "redefinition of alternate col set `%s'.\n" key);
    let names = Wrstrm.parse_strings strm in
    let cols = (List.map (fun n ->
			    n, (Queue.create ()))
		  names) in
      Hashtbl.add alts key cols


let parse_alt_row_4 alts line =
  Verb.pe Verb.debug "parse altcol v4 |%s|\n%!" line;
  let strm = stream_after_prefix alt_row_prefix_4 line in
  let key = Wrstrm.parse_string strm in
  let nums = Wrstr.parse_floats (Wrstrm.rest_string strm) in
    try
      let cols = Hashtbl.find alts key in
	if not ((List.length cols) = (List.length nums)) then
	  failwith "wrong number of numbers in an alt row";
	List.iter2 (fun x (_, q) ->
		      Queue.add x q)
	  nums cols
    with Not_found ->
      Verb.pe Verb.always
	"parse_alt_row_4: Unable to find altcols key: %s\n%!" key;
      raise Not_found


let alt_cols cols alts =
  (** adds to cols *)
  Hashtbl.iter (fun _ acols ->
		  List.iter (fun (key, q) ->
			       assert (not (Hashtbl.mem cols key));
			       Hashtbl.add cols key (Wrarray.of_queue q))
		    acols)
    alts;
  cols


let read_4 name ch =
  (* start line already read *)
  let vals = Hashtbl.create 16
  and alts = Hashtbl.create 16
  and cols = ref [] in
  let rec read_next () =
    let line = (try
		  input_line ch
		with End_of_file -> failwith "unexpected EOF in datafile") in
      if line = end_4
      then ()
      else
	(if Wrstr.starts_as pair_prefix_4 line
	 then parse_val_4 vals line
	 else if Wrstr.starts_as cols_prefix_4 line
	 then (match !cols with
		   [] -> cols := parse_col_names_4 line
		 | _ -> failwith "already got col names")
	 else if Wrstr.starts_as alt_cols_prefix_4 line
	 then parse_alt_col_names_4 alts line
	 else if Wrstr.starts_as alt_row_prefix_4 line
	 then parse_alt_row_4 alts line
	 else if ((line <> "") ||
		    (String.contains "0123456789. \t+-" line.[0]))
	 then (match !cols with
		   [] ->
		     failwith (Wrutils.str "cols arrived before names: [%s]"
				 line)
		 | _ -> parse_row !cols line)
	 else
	   failwith (Wrutils.str "bad line in data format 4 file: %s\n" line);
	 read_next ())
  in
    read_next ();
    { name = name;
      vals = vals;
      cols = alt_cols (from_cols !cols) alts; }


(******* format-agnostic high-level functions *******)


let read name ch =
  let line = input_line ch in
    if Wrstr.starts_as val_prefix_3 line
    then read_3 name line ch
    else if line = start_4
    then (Verb.pe Verb.debug "reading v4 dfile\n%!";
	  read_4 name ch)
    else
      failwith (Wrutils.str "unknown file format.  starts with: %s" line)


let load ?(check_trailer = false) filename =
  try
    let df = Wrio.with_infile filename (read filename) in
      if check_trailer && (not (has_val df "wall finish time")) then
	failwith (Wrutils.str "can't find \"wall finish time\" in \"%s\" - is it incomplete?" filename);
      df
  with Failure s -> failwith (Wrutils.str "Failed on %s: %s" filename s)
    | e ->
(*
  (* won't compile in 3.10. *)
	Printexc.print_backtrace stdout;
*)
	Verb.pr Verb.always
	  "Datafile.load exception: %s on %s\n%!" (Printexc.to_string e)
	  filename;
	raise e


let seems_complete filename =
  let s = "#end" in
    try
      Wrio.with_infile filename
	(fun ch ->
	   Wrio.skip_through_str ch s;
	   true)
    with End_of_file -> false


let run_finished filename =
  (seems_complete filename) &&
    let s = "yes" in
      try
	Wrio.with_infile filename
	  (fun ch ->
	     Wrio.skip_through_str ch s;
	     true)
      with End_of_file -> false


(************************ writing *************************)


type pairs = (string * string) list


let write_pair ch (k, v) =
  Wrutils.pf ch "%s \"%s\"\t\"%s\"\n"
    pair_prefix_4 (String.escaped k) (String.escaped v)


let write_pairs ch pairs =
  List.iter (write_pair ch) pairs


let write_colnames ch names =
  match names with
    first::rest ->
      Wrutils.pf ch "%s \"%s\"" cols_prefix_4 (String.escaped first);
      List.iter (fun n -> Wrutils.pf ch "\t\"%s\"" (String.escaped n))
	rest;
      Wrutils.pf ch "\n"
  | [] -> ()


let write_alt_colnames ch name names =
  Wrutils.pf ch "%s \"%s\"" alt_cols_prefix_4 (String.escaped name);
  List.iter (fun n -> Wrutils.pf ch "\t\"%s\"" (String.escaped n))
    names;
  Wrutils.pf ch "\n"


let write_alt_row_prefix ch name =
  Wrutils.pf ch "%s \"%s\"\t" alt_row_prefix_4 (String.escaped name)


let header_pairs () =
  [ "wall start date", Wrsys.time_string ();
    "wall start time", string_of_float (Unix.gettimeofday ());
    "machine id", (Wrsys.machine_id ()) ]


let trailer_pairs () =
  [ "wall finish time", string_of_float (Unix.gettimeofday ());
    "wall finish date", Wrsys.time_string (); ]


let write_header_pairs ch =
  Wrio.output_line ch start_4;
  List.iter (write_pair ch) (header_pairs ())


let write_trailer_pairs ch =
  List.iter (write_pair ch) (trailer_pairs ());
  Wrio.output_line ch end_4


let test () =
  Wrio.with_outfile "/tmp/foo"
    (fun ch ->
       write_header_pairs ch;
       write_pairs ch ["a", "b"];
       write_colnames ch ["col1"; "col2"];
       Wrutils.pf ch "0 1\n";
       write_alt_colnames ch "error logging" ["val"];
       write_alt_row_prefix ch "error logging";
       Wrutils.pf ch "6\n";
       write_trailer_pairs ch);
  let df = load "/tmp/foo" in
    Wrutils.pr "a = `%s'\n" (get_val df "a");
    Wrutils.pr "end date = `%s'\n" (get_val df "wall finish date");
    Wrutils.pr "col2.(0) = %f\n" (get_col df "col2").(0);
    Wrutils.pr "val.(0) = %f\n" (get_col df "val").(0)


(************ piping data to a datafile ****************)


let extract_float_val prefix line =
  (** extract float from inside quotes after the given prefix *)
  try
  float_of_string (Wrstr.peel_and_chop
		     ~n:((String.length prefix) + 2)
		     ~m:1 line)
  with Failure str -> failwith (Wrutils.str "%s: %s" str (Wrstr.peel_and_chop
							  ~n:((String.length prefix) + 2)
							  ~m:1 line))


let pipe_data inch outch =
  (** pipes from [inch] to [outch], flushing line-by-line and looking
      for "final sol cost" and "total raw cpu time" pairs, which are
      returned as a pair of floats after EOF *)
  let q = ref None
  and t = ref None in
    (let cost = pair_prefix_4 ^ " \"final sol cost\""
     and time = pair_prefix_4 ^ " \"total raw cpu time\"" in
       try
	 while true do
	   let line = input_line inch in
	     Verb.pe Verb.debug "Pipe_data: %s\n" line;
	     Wrio.output_line outch line;
	     flush outch;
	     if (Wrstr.starts_as cost line) then
	       q := Some (extract_float_val cost line)
	     else if (Wrstr.starts_as time line) then
	       t := Some (extract_float_val time line)
	 done
       with End_of_file -> ());
    match !q with
	None -> failwith "no final cost line seen!"
      | Some q ->
	  match !t with
	      None -> failwith "no final raw time line seen!"
	    | Some t -> q,t


(************ calling a stand-alone solver ****************)


let call_solver solver args prob_path attrs outch =
  Wrsys.with_subprocess_pipes
    (Wrutils.str "%s %s < %s" solver args prob_path)
    (fun to_solver from_solver ->
       write_header_pairs outch;
       write_pairs outch attrs;
       write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
       let res = pipe_data from_solver outch in
	 write_trailer_pairs outch;
	 res)


let do_run solver args prob_path data_root attrs =
  let run_file = Rdb.path_for data_root
    (Rdb.override_attrs ["type", "run"] attrs) in
    if ((Sys.file_exists run_file) &&
	  (seems_complete run_file)) then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative data_root
					    prob_path);
	   let c,t = call_solver solver args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)


(* EOF *)
