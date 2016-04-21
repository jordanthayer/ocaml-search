(* Build an anytime profile for an anytime algorithm. *)

open Fn

let qualkey = "sol cost"
let timekey = "raw cpu time"

(** Load data for the given instance and algorithm attributes. *)
let data domain iattrs aattrs =
  Dataset.load_from_rdb_with_domain ~domain (iattrs @ aattrs) ~name:"prof"

type ent = {
  vec : int array;
  mutable cnt : int;
}

type tab = ent array

let qind ~q0 ~dq q = truncate ((q -. q0) /. dq)

let inst ~q0 ~dq ~dt tab inst =
  Printf.printf "\tinstance %s\n%!" (Dataset.name inst);
  let keys = [| timekey; qualkey; |] in
  let fl = float_of_string and sort = Dataset.Ascending in
  let rows = Dataset.get_row_vector ~sort fl keys inst in
  let rec scan q t i =
    if i < Array.length rows then begin
      let t' = rows.(i).(0) and q' = rows.(i).(1) in
      let q_i = qind ~q0 ~dq q and q'_i = qind ~q0 ~dq q' in
      tab.(q_i).cnt <- tab.(q_i).cnt + 1;
      if t >= t' then begin
	tab.(q_i).vec.(q'_i) <- tab.(q_i).vec.(q'_i) + 1;
	scan q' (t +. dt) (i + 1)
      end else begin
	tab.(q_i).vec.(q_i) <- tab.(q_i).vec.(q_i) + 1;
	scan q (t +. dt) i
      end
    end in
  scan infinity 0. 0

(** Get a normalized probability table. *)
let probs tab =
  let pr ent =
    let cnt = float ent.cnt in
    Array.map (fun c -> float c /. cnt) ent.vec in
  Array.map pr tab

type prof = {
  pr : float array array;
  q0 : float;
  dq : float;
  dt : float;
  qmax : float;
  tmax : float;
  qn : int;
  tn : int;
}

(** Learn a profile from the given dataset. *)
let prof ~qn ~tn dset =
  let q0 = Dataset.get_min qualkey dset in
  let qmax = Dataset.get_max qualkey dset in
  let tmax = Dataset.get_max timekey dset in
  let dt = tmax /. float (tn - 1) in
  let dq = (qmax -. q0) /. float (qn - 1) in
  let tab = Array.init qn (fun _ -> { vec = Array.create qn 0; cnt = 0 }) in
  let insts = Dataset.group_by [|"num"|] dset in
  Printf.printf "tmax=%g\n" tmax;
  Printf.printf "dt=%g\n" dt;
  Printf.printf "q0=%g\n" q0;
  Printf.printf "qmax=%g\n" qmax;
  Printf.printf "dq=%g\n" dq;
  List.iter (inst ~q0 ~dq ~dt tab) insts;
  { pr = probs tab; q0 = q0; dq = dq; dt = dt; qn = qn; tn = tn;
    qmax = qmax; tmax = tmax; }

let solve prof util =
  let qn = prof.qn and tn = prof.tn in
  let v = Array.make_matrix qn tn 0. in
  let stop = Array.make_matrix qn tn false in
  let expt ~qind ~tind =
    let sum = ref 0. in
    for j = 0 to qn - 1 do
      let vl = v.(j).(tind + 1) in
      let pr = prof.pr.(qind).(j) in
      if vl <> 0. && pr > 0. then sum := !sum +. pr *. vl
    done;
    !sum in
  let tmax = prof.dt *. float (tn - 1) in
  for qind = 0 to qn - 1 do
    let q = prof.q0 +. prof.dq *. float qind +. prof.dq /. 2. in
    v.(qind).(tn - 1) <- util ~q ~t:(tmax -. prof.dt /. 2.);
    stop.(qind).(tn - 1) <- true;
  done;
  for qind = 0 to qn - 1 do
    let q_i = prof.q0 +. prof.dq *. float qind +. prof.dq /. 2. in
    let stop = stop.(qind) and v = v.(qind) in
    for tind = tn - 2 downto 0 do
      let t = prof.dt *. float tind +. prof.dt /. 2. in
      let u = util ~q:q_i ~t in
      let sum = expt ~qind ~tind in
      v.(tind) <- Math.fmax sum u;
      stop.(tind) <- sum < u
    done
  done;
  stop, v

let save domain iattrs aattrs prof =
  let root = Filename.concat User_paths.data_root domain in
  let df = Rdb.path_for root (iattrs @ aattrs) in
  Wrio.with_outfile df (fun ch -> Marshal.to_channel ch prof [])

let load df =
  Wrio.with_infile df (fun ch -> (Marshal.from_channel ch : prof))

let go init delta dq dt =
  let iattrs =
    ["heuristic", "manhattan"; "obstacles", "uniform"; "moves", "Four-way";
     "costs", "Unit"; "prob", "0.35"; "width", "5000"; "height", "5000"; ] in
  let aattrs =
    [ "init wt", string_of_float init; "delta wt", string_of_float delta] in
  let dat = data "grid" iattrs (("alg", "arastar.training") :: aattrs) in
  Printf.printf "Loaded the data\n%!";
  let prof = prof dq dt dat in
  Printf.printf "Built the profile\n%!";
  let savattrs =
    ["alg", "arastar.prof"; "qual samples", string_of_int dq;
     "time samples", string_of_int dt ] in
  save "grid" iattrs (savattrs @ aattrs) prof


(** Build a monitor for the given utility function using the given
    anytime profile.  The result is the stopping function and the
    frequency (in seconds) at which it should be called). *)
let mon prof_file util =
  let prof = load prof_file in
  let stop, v = solve prof util in
  let tmax = prof.tmax in
  let q0 = prof.q0 and qmax = prof.qmax in
  let dq = prof.dq and dt = prof.dt in
  (fun q t ->
    if q < q0 || t > tmax then
      true
    else begin
      let q = if q > qmax then qmax else q in
      let q_i = truncate ((q -. q0) /. dq) in
      let t_i = truncate (t /. dt) in
      stop.(q_i).(t_i)
    end), dt
