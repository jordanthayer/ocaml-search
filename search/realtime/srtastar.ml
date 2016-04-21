(* Simplified RTA*: a more complicated version of RTA* from "Do the
   Right Thing," Russell and Wefald 1991. *)

let rec srtastar lim eval succs key is_goal ~seen ~gen cost state =
  if not (Limit.halt_p lim) then
    if is_goal state then
      Limit.new_incumbent lim (Limit.Incumbent (cost, state))
    else begin
      let kids = succs state 0. in
      Limit.incr_exp lim;
      Limit.incr_gen_n lim (List.length kids);
      let mins = ref [] and minf = ref infinity in
      Htable.clear gen;
      let consider_kid ((k, c) as kid) =
	let f = eval !minf k +. c in
	if f < !minf then begin
	  minf := f;
	  mins := [kid];
	end else if f = !minf then
	  mins := kid :: !mins in
      List.iter consider_kid kids;
      if !mins <> [] then begin
	Htable.replace seen (key state) (!minf *. 1.1);
	let len = List.length !mins in
	let n = if len = 1 then 0 else Random.int len in
	let k, c = List.nth !mins n in
	srtastar lim eval succs key is_goal ~seen ~gen (cost +. c) k
      end
    end

let eval seeerr is_goal succs h lim dlim key ~seen ~gen alpha state =

  let rec dfs ~fback ~alpha ~g depth ~state =
    if is_goal state then
      g
    else begin
      let k = key state in
      let c = try Htable.find seen k with Not_found -> neg_infinity in
      let f = g +. Math.fmax (h state) c in
      if depth < dlim
	&& f < alpha
	&& not (Limit.halt_p lim)
	&& not (Htable.mem seen k)
      then begin
	let ss = succs state g in
	Limit.incr_exp lim;
	Limit.incr_gen_n lim (List.length ss);
	let fback = best_kid ~fback ~alpha ~g (depth + 1) ss in
	begin match seeerr with
	  | None -> ()
	  | Some fn when Random.float 1. < 0.1 ->
	    fn (h state) (dlim - depth) (fback -. f)
	  | _ -> ()
	end;
	fback
      end else
	Math.fmin fback f
    end

  and best_kid ~fback ~alpha ~g depth' = function
    | (k, g') :: ks ->
      let f = dfs ~fback ~alpha ~g:g' depth' ~state:k in
      let fback' = Math.fmin fback f in
      let alpha' = Math.fmin fback' alpha in
      best_kid ~fback:fback' ~alpha:alpha' ~g depth' ks
    | [] -> fback in

  try
    let h = Htable.find seen (key state) in
    Limit.incr_dups lim;
    h
  with Not_found ->
    let h = dfs ~fback:infinity ~alpha ~g:0. 0 ~state in
    h

let search results unwrap sface args =
  let dlim = Search_args.get_int "Srtastar.search" args 0 in
  Random.self_init ();
  let h = sface.Search_interface.h in
  let limits = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let succs = sface.Search_interface.domain_expand in
  let init = sface.Search_interface.initial in
  let seen = Htable.create hash eq 149 in
  let gen = Htable.create hash eq 149 in
  srtastar limits (eval None is_goal succs h limits dlim key ~seen ~gen)
    succs key is_goal ~seen ~gen 0. init;
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results limits)

let dups sface args =
  search Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  search Limit.results5 Limit.unwrap_sol5 sface args

let errtab errfile =
  if not (Sys.file_exists errfile) then
    [||]
  else
    Wrio.with_infile errfile
      (fun inch -> (Marshal.from_channel inch : int array array array))

let saveerr errfile errtab =
  let num = ref 0 in
  for i = 0 to Array.length errtab - 1 do
    for j = 0 to Array.length errtab.(i) - 1 do
      for k = 0 to Array.length errtab.(i).(j) - 1 do
	num := !num + errtab.(i).(j).(k)
      done
    done;
  done;
  Wrio.with_outfile errfile
    (fun outch -> Marshal.to_channel outch errtab [])

let seeerr errtab h dd df =
(*
  let h = 0 in
*)
  let h = truncate h in
  let hsize = Array.length !errtab in
  if hsize <= h then begin
    let hsize' = (h + 1) * 2 in
    let hent i = if i < hsize then !errtab.(i) else [||] in
    errtab := Array.init hsize' hent;
  end;
  assert (Array.length !errtab > h);
  let dsize = Array.length !errtab.(h) in
  if dsize <= dd then begin
    let dsize' = (dd + 1) * 2 in
    let dent i = if i < dsize then !errtab.(h).(i) else [||] in
    !errtab.(h) <- Array.init dsize' dent;
  end;
  assert (Array.length !errtab.(h) > dd);
  let df = truncate df in
  let fsize = Array.length !errtab.(h).(dd) in
  if fsize <= df then begin
    let fsize' = (df + 1) * 2 in
    let fent i = if i < fsize then !errtab.(h).(dd).(i) else 0 in
    !errtab.(h).(dd) <- Array.init fsize' fent;
  end;
  assert (df >= 0);
  assert (Array.length !errtab.(h).(dd) > df);
  !errtab.(h).(dd).(df) <- !errtab.(h).(dd).(df) + 1

let appenderr results unwrap sface args =
  let dlim = Search_args.get_int "Srtastar.search" args 0 in
  let errfile = Search_args.get_string "Srtastar.search" args 1 in
  Random.self_init ();
  let h = sface.Search_interface.h in
  let limits = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let succs = sface.Search_interface.domain_expand in
  let init = sface.Search_interface.initial in
  let seen = Htable.create hash eq 149 in
  let gen = Htable.create hash eq 149 in
  let errtab = ref (errtab errfile) in
  srtastar limits
    (eval (Some (seeerr errtab)) is_goal succs h limits dlim key ~seen ~gen)
    succs key is_goal ~seen ~gen 0. init;
  saveerr errfile !errtab;
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results limits)

let dtaserr_dups sface args =
  appenderr Limit.results6 Limit.unwrap_sol6 sface args

let dtaserr_no_dups sface args =
  appenderr Limit.results5 Limit.unwrap_sol5 sface args
