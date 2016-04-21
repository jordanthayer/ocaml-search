(**

    @author jtd7
    @since 2010-09-08

    Calculating, Writing, Loading Pattern Databases for Unit Cost Sliding Tiles
*)
open Sliding_tiles_pdb
module STI = Sliding_tiles_inst
module ST = Sliding_tiles
module MST = Macro_sliding_tiles

type pattern = int list


let make_h cost (key, size) =
  let info = Packed_ints.info size in
  let get = Packed_ints.make_get info cost in
  (fun n -> float (get (key n)))

(************************ Generation Functions********************************)
let make_pdb pdb_size state_size add pkey skey pattern tile_cost inst move =
  (** solves the problem backwards, writing down the pdb *)
  let inst = STI.reverse_inst inst in
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info
  and set = Packed_ints.make_set info
  and current = ref 0
  and last_p = ref 0 in
  let exp = match move with
    | "standard" -> pdb_expand add pattern inst tile_cost info get set
    (*| "macro" -> macro_pdb_expand add pattern inst tile_cost get set h d*)
    | _ -> failwith "Movement not found" in
  let initial = ST.make_initial inst create set (fun _ -> 0.) (fun _ -> 0) in
  let initial = initial.ST.contents in
  let expand state cost =
    current := !current + 1;
    let p = !current * 100 / pdb_size in
      if p > !last_p && (p mod 5) = 0
      then (last_p := p;
	    Verb.pe Verb.always "%i%% Complete\n" p); exp state cost in
    Verb.pe Verb.always "Generating the packed ints pdb\n%!";
    let pkey, punrank = pkey inst
    and skey, sunrank = skey inst in
    Packed_int_pdb.dups pdb_size ~state_size expand
      pkey ~search_key:skey punrank ~search_unrank:sunrank
      initial (=) Fn.identity



let generate_pdb add pattern cost move problem =
  (** pattern is a list of tiles to be made *)
  let has_0 = List.mem 0 pattern in
  let cols = problem.STI.ncols
  and rows = problem.STI.nrows
  and pkey = make_gen_pdb_key pattern in
  let size = rows * cols in
  let skey =  make_gen_pdb_key (if has_0 then pattern
					  else (0::pattern)) in
  let pdb_size = fst (List.fold_left
			(fun (accum,sz) _ ->
			   (accum * sz), sz - 1) (1,size) pattern) in
  let state_size = if has_0 then pdb_size else
    fst (List.fold_left
	   (fun (accum,sz) _ ->
	      (accum * sz), sz - 1) (1,size) (0::pattern)) in
    Verb.pe Verb.debug "Pdb size: %i\n" pdb_size;
  let pdb,sz = (make_pdb pdb_size state_size add pkey skey pattern
		  (snd cost) problem move) in
    Verb.pe Verb.debug "Pdb generated\n";
    (path ~add:add rows cols (fst cost) move pattern), (pattern,pkey,pdb,sz)


(******************************* I/O ******************************************)

let write_pdb ch pdb_pair =
  Marshal.to_channel ch pdb_pair []


let read_pdb ch =
  Marshal.from_channel ch


let pdb_to_path path (pattern,pkey,pdb,sz) =
  let pdb_pair = pattern,pdb,sz in
    Wrfname.ensure_path path;
    if not (Sys.file_exists path)
    then Wrio.with_outfile path (fun ch -> write_pdb ch pdb_pair)


let pdb_from_path path =
  (** Returns a tuple of the pattern, pdb_key and the pdb, in that order
      from the file.  The heuristics must be generated after the pdb is
      lodade *)
  if Sys.file_exists path
  then (let pattern,pdb,sz = Wrio.with_infile path read_pdb in
	  (pattern,pdb),sz)
  else failwith (Wrutils.str "No PDB @ %s" path)


let make_and_save prob add pattern cost movement =
  let path, pdb_trip = generate_pdb add pattern cost movement prob in
    Wrfname.ensure_path path;
    pdb_to_path path pdb_trip


(* EOF *)
