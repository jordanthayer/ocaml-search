(** Converts a DIMACS format road map into a binary format that may be
    easier to read.

    @author eaburns
    @since 2010-08-18
*)

open Printf

let load_coordinates path =
  (** [load_coordinates path] loads the coordinates. *)
  let coord = Filename.concat path "coordinates.co" in
    Verb.pr Verb.always "done\nLoading coordinates... %!";
    let c, time =
      Wrsys.with_time (fun () -> Dimacs.load_coordinates coord)
    in
      Verb.pr Verb.always "done\n\t%d points (%f seconds)\n%!"
	c.Dimacs.c_nnodes time;
      c


let load_graph sort path =
  (** [load_graph sort path] loads the given graph. *)
  let gr_file = Filename.concat path (sprintf "%s.gr" sort) in
    Verb.pr Verb.always "Loading %s graph... %!" sort;
    let g, time =
      Wrsys.with_time (fun () -> Dimacs.load_graph gr_file)
    in
      Verb.pr Verb.always "done\n\t%d arcs (%f seconds)\n"
	g.Dimacs.narcs time;
      g


(*
let main () =
  let path = Sys.argv.(1) and sort = Sys.argv.(2) and outfile = Sys.argv.(3) in
  let coords = load_coordinates path in
  let wts = load_graph sort path in
  let nnodes = coords.Dimacs.c_nnodes in
    Wrio.with_outfile outfile (fun outch ->
				 Roadmap.Binary_format.output_hdr outch nnodes;
				 for n = 1 to nnodes do
				   Dimacs.Binary_format.output_node
				     outch coords ~wts n
				 done)
*)

let main () =
  let path = Sys.argv.(1) and sort = Sys.argv.(2) and outfile = Sys.argv.(3) in
  let coords = load_coordinates path in
  let wts = load_graph sort path in
  let t = Dimacs.to_roadmap coords wts in
    Roadmap.Binary_format.save t outfile


let _ = main ()
