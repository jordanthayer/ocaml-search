(* DTA*: ``Do the Right Thing,'' Russell and Wefald, 1991. *)

let debug = false

open Float_ref

type 'state node = {
  mutable st : 'state;
  h : float;
  mutable g : float;
  mutable f : float;
  mutable ndpos : int;
}

let ndbetter a b =
  if a.f = b.f then a.g > b.g else a.f < b.f

let ndequal a b =
  a.f = b.f && a.g = b.g

let ndupdate a ind =
  a.ndpos <- ind

(* Set of leaves beneath a move that is currently under
   consideration. *)
type ('key, 'state) move = {
  mv : 'state;
  cst : float;
  opn : 'state node Dpq.t;
  cls : ('key, 'state node) Htable.t;
  mutable minf : float;
  mutable lfpos : int;
}

let lfbetter a b =
  if a.minf = b.minf then begin
    if Dpq.empty_p b.opn then
      true
    else if Dpq.empty_p a.opn then
      false
    else
      let a = Dpq.peek_first a.opn and b = Dpq.peek_first b.opn in
      ndbetter a b
  end else
    a.minf < b.minf

let lfequal a b =
  a.minf = b.minf && ndequal (Dpq.peek_first a.opn) (Dpq.peek_first b.opn)

let lfupdate a ind =
  a.lfpos <- ind

let nopos = Dpq.no_position

(* r is the tradeoff between cost/time

   pr is the cumulative probability table (Pr[f^d(x) > x | h])

   br is the mean branching factor. *)
let dtas info key pkey eq hash heur succs isgoal dhoriz grain init r pr br =

  (* Stored h values *)
  let tbl = Htable.create hash eq 149 in

  let h n =
    try Htable.find tbl (key n) with Not_found -> heur n in

  let node st g =
    let h = h st in
    { st = st; h = h; g = g; f = g +. h; ndpos = nopos } in

  (* Makes a prio queue of the moves currently under consideration.
     For each move, we maintain a set of leaves, minf and 'closed'
     nodes for this round of search. *)
  let mkmoves kids =
    let mkopn nd =
      let opn = Dpq.create ndbetter ndupdate 100 nd in
      Dpq.insert opn nd;
      opn in
    let mkmove (mv, c) =
      let nd = node mv c in
      let cls = Htable.create hash eq 149 in
      { opn = mkopn nd; cls = cls; mv = mv; cst = c; minf = nd.f;
	lfpos = nopos; } in
    match List.map mkmove kids with
      | [] ->
	invalid_arg "Dtas.mkmoves: no kids"
      | (l :: _) as ls ->
	let pq = Dpq.create lfbetter lfupdate 100 l in
	List.iter (Dpq.insert pq) ls;
	pq in

  let ndupdate alpha n g' st' =
    n.f <- n.f -. n.g +. g';
    n.g <- g';
    n.st <- st';
    if n.ndpos = nopos then
      Dpq.insert alpha.opn n
    else
      Dpq.see_update alpha.opn n.ndpos in

  let expand alpha bst =
    let kids = succs bst.st bst.g in
    Limit.incr_exp info;
    Limit.incr_gen_n info (List.length kids);
    let kid (k, g) =
      let ky = key k in
      try
	let other = Htable.find alpha.cls ky in
	if other.g > g then ndupdate alpha other g k
      with Not_found ->
	let nd = node k g in
	Dpq.insert alpha.opn nd;
	Htable.add alpha.cls ky nd in
    List.iter kid kids in

  let backuph moves move =
    let minf' = try (Dpq.peek_first move.opn).f with Dpq.Empty _ -> infinity in
    if move.minf < minf' then begin
      if debug then
	Printf.printf "minf=%g, minf'=%g,\n%s%!" move.minf minf'
	  (pkey (key move.mv));
      move.minf <- minf';
      Htable.replace tbl (key move.mv) (minf' -. move.cst);
      Dpq.see_update moves move.lfpos
    end in

  (* Expand the best node beneath the best leaf, backing up the
     heuristic estimate. *)
  let search moves =
    let i = ref 0 and goal = ref None in
    while !i < grain && !goal = None && not (Limit.halt_p info) do
      let alpha = Dpq.peek_first moves in
      let bst = Dpq.extract_first alpha.opn in
      if isgoal bst.st then
	goal := Some alpha
      else begin
	expand alpha bst;
	backuph moves alpha;
      end;
      incr i;
    done;
    !goal in

  (* Initial set of leaves with f values less than [fbeta1]. *)
  let m0 ls fbeta1 =
    let m = ref [] in
    while not (Dpq.empty_p ls) && (Dpq.peek_first ls).f < fbeta1 do
      m := Dpq.extract_first ls :: !m
    done;
    m in

  let q d n x =
    (*
    let h = 0 in
    *)
    let h = truncate (heur n.st) in
    let ef = truncate (x -. n.f) in
    if h >= Array.length pr || d >= Array.length pr.(h)
      || ef >= Array.length pr.(h).(d)
    then
      0.
    else if ef < 0 then
      1.
    else
      pr.(h).(d).(ef) in

  let value fbeta1 dlta cst m d =
    let sum = float_ref 0. in
    let q = q d in
    let x = float_ref fbeta1 in
    assert (dlta >= fbeta1);
    while !!x < dlta do
      let qm = List.fold_left (fun m n -> Math.fmin m (q n !!x)) infinity m in
      if debug then Printf.printf "qm=%g\n" qm;
      sum <-- !!sum +. qm;
      x <-- !!x +. 1.;
    done;
    !!sum in

  (* Select alpha and beta1. In the case of ties, choose at random. *)
  let alphabeta moves =
    let alpha = Dpq.peek_first moves in
    let beta1 = Dpq.peek_second moves in
    (*
      if lfequal beta1 alpha then begin
      let mins = ref [] in
      let ismin m = if lfequal m alpha then mins := m :: !mins in
      Dpq.iter ismin moves;
      let len = List.length !mins in
      assert (len >= 2);
      let n = Random.int len in
      let alpha, mins = Wrlist.takenth n !mins in
      let m = Random.int (len - 1) in
      let beta1 = List.nth mins m in
      alpha, beta1
      end else
    *)
    alpha, beta1 in

  (* Decide if we should continue searching by performing a meta-level
     (parameter space) search. *)
  let continue moves =
    let alpha, beta1 = alphabeta moves in
    let fbeta1 = beta1.minf in
    let lvs = Dpq.copy_no_notif alpha.opn in
    let m = m0 lvs beta1.minf in
    let cont = ref false in
    if debug then Printf.printf "continue? %d\n" info.Limit.expanded;
    while not !cont && Dpq.size lvs > 1 do
      let mcard = float (List.length !m) in
      let dlta = (Dpq.peek_first lvs).f in
      if debug then
	Printf.printf "\tmcard=%g, fbeta1=%g, dlta=%g, r=%g, br=%g\n%!"
	  mcard fbeta1 dlta r br;
      let d = ref 0 in
      while !m <> [] && not !cont && !d < dhoriz do
	let cst = r *. mcard *. br ** float !d in
	let vl = value fbeta1 dlta cst !m !d in
	if debug then
	  Printf.printf "\t\td=%d, dhoriz=%d, vl=%g, cst=%g\n%!"
	    !d dhoriz vl cst;
	cont := vl -. cst >= 0.;
	incr d
      done;
      m := Dpq.extract_first lvs :: !m;
    done;
    !cont in

  let prmoves moves =
    let lst = ref [] in
    Dpq.iter (fun s -> lst := s :: !lst) moves;
    let cmp a b =
      if lfbetter a b then ~-1 else if lfbetter b a then 1 else 0 in
    let srtd = List.sort cmp !lst in
    if srtd <> [] then Printf.printf "----------\n";
    List.iter (fun s ->
      Printf.printf "minf=%g\n%s" s.minf (pkey (key s.mv)))
      srtd in

  (* Choose the next move. *)
  let dtas_choose cur =
    let kids = (succs cur.st 0.) in
    Limit.incr_exp info;
    Limit.incr_gen_n info (List.length kids);
    let moves = mkmoves kids in
    let worth = ref true and goal = ref None in
    while !worth && !goal = None && not (Limit.halt_p info) do
      if debug then
	prmoves moves;
      goal := search moves;
      if !goal = None then
	worth := Dpq.size moves > 1 && continue moves;
    done;
    let alpha = match !goal with
      | Some alpha -> alpha
      | None -> Dpq.peek_first moves in
    alpha.mv, alpha.cst, alpha.minf in

  (* Continually choose moves until we reach the goal. *)
  let rec dtas_solve cost cur =
    if debug then
      Printf.printf "step (h=%g)\n%s%!" (h cur) (pkey (key cur));
    if isgoal cur then
      Limit.new_incumbent info (Limit.Incumbent (cost, cur))
    else begin
      let nxt, cst, f = dtas_choose (node cur 0.) in
      Htable.replace tbl (key cur) (f *. 1.1);
      if not (Limit.halt_p info) then dtas_solve (cost +. cst) nxt
    end in

  dtas_solve 0. init

let dumppr pr =
  let fmth fmt h =
    for dd = 0 to Array.length pr.(h) - 1 do
      for err = 0 to Array.length pr.(h).(dd) - 1 do
	Format.fprintf fmt "(@[%d %d %f@])@ " dd err pr.(h).(dd).(err);
      done
    done in
  let oc = open_out "pr.spt" in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "(@[let* (@[";
  for h = 0 to Array.length pr - 1 do
    if pr.(h) <> [||] then begin
      Format.fprintf fmt "(@[dat%d@ (@[%a@])@])@\n" h fmth h;
      Format.fprintf fmt
	"(@[ds%d@ (heatmap-dataset@ @[:bin-size (1 1)@ :triples dat%d@])@])@\n"
	h h;
      Format.fprintf fmt
	"(@[pl%d (num-by-num-plot@ :title \"%d\"@ :x-label \"delta d\"@ " h h;
      Format.fprintf fmt
	":y-label \"heuristic error\"@ :dataset ds%d)@])@\n" h;
    end
  done;
  Format.fprintf fmt "@])@\n(display";
  for h = 0 to Array.length pr - 1 do
    if pr.(h) <> [||] then Format.fprintf fmt "@ pl%d" h
  done;
  Format.fprintf fmt ")@])@.";
  close_out oc


(* Load the learned errors from the given file and build a model of
   cumulative f-error probabilities. *)
let errcdf efile =
  let ld inch = (Marshal.from_channel inch : int array array array) in
  let tab = Wrio.with_infile efile ld in
  let sum2d mat =
    float (Array.fold_left (fun s a -> Array.fold_left (+) s a) 0 mat) in
  let probs hsum ary =
    let pr = Array.map (fun v -> float v /. hsum) ary in
    for i = Array.length ary - 2 downto 0 do
      pr.(i) <- pr.(i) +. pr.(i + 1);
    done;
    pr in
  let pr = Array.map (fun mat -> Array.map (probs (sum2d mat)) mat) tab in
  dumppr pr;
  pr

let search results unwrap sface args =
  let dhoriz = Search_args.get_int "Dtas.search" args 0 in
  let grain = Search_args.get_int "Dtas.search" args 1 in
  let br = Search_args.get_float "Dtas.search" args 2 in
  let r = Search_args.get_float "Dtas.search" args 3 in
  let efile = Search_args.get_string "Dtas.search" args 4 in
  Random.self_init ();
  let h = sface.Search_interface.h in
  let info = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let isgoal = sface.Search_interface.goal_p in
  let succs = sface.Search_interface.domain_expand in
  let init = sface.Search_interface.initial in
  let pkey = sface.Search_interface.key_printer in
  let cdf = errcdf efile in
  dtas info key pkey eq hash h succs isgoal dhoriz grain init r cdf br;
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results info)

let dups sface args =
  search Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  search Limit.results5 Limit.unwrap_sol5 sface args
