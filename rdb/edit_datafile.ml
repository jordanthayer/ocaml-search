(** Performs simple edits on a datafile.

    @author eaburns
    @since 2010-10-14
*)


let remove_matching_lines root ?(attrs=[]) regexp =
  (** [remove_matching_lines root ?attrs regexp] removes lines
      matching the given regular expression. *)
  let files = Rdb.matching_paths root [] in
  let fixed = ref 0 in
  let seen = ref 0 in
  let handle_file f =
    let lines = Wrio.with_infile f Wrio.input_lines in
    let keep =
      List.filter (fun l -> not (Str.string_match regexp l 0)) lines
    in
      incr seen;
      if keep <> lines then begin
	Sys.rename f (f^"~");
	Wrio.with_outfile f
	  (fun oc -> List.iter (Printf.fprintf oc "%s\n") keep);
	incr fixed;
      end
  in
    List.iter handle_file files;
    Printf.printf "examined %d files, fixed %d\n" !seen !fixed


let remove_duplicate_pairs root ?(attrs=[]) =
  (** [remove_duplicate_pairs root ?attrs] removes duplicate pairs
      from datafiles. *)
  let files = Rdb.matching_paths root [] in
  let fixed = ref 0 in
  let seen = ref 0 in
  let handle_file f =
    let lines = Wrio.with_infile f Wrio.input_lines in
    let tbl = Hashtbl.create 100 in
      List.iter (fun l -> Hashtbl.replace tbl l true) lines;
      let keep =
	List.filter (fun l ->
		       if Hashtbl.mem tbl l then begin
			 Hashtbl.remove tbl l;
			 true
		       end else
			 false)
	  lines
      in
	incr seen;
	if keep <> lines then begin
	  Sys.rename f (f^"~");
	  Wrio.with_outfile f
	    (fun oc -> List.iter (Printf.fprintf oc "%s\n") keep);
	  incr fixed;
	end
  in
    List.iter handle_file files;
    Printf.printf "examined %d files, fixed %d\n" !seen !fixed
