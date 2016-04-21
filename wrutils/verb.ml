(* $Id: verb.ml,v 1.2 2003/11/20 17:39:51 ruml Exp ruml $

*)

let always   = 1
and often    = 2
and optional = 3
and toplvl   = 4
and debug    = 5
and never    = 6

(** 1 through 5, with 5 being most verbose *)
let current = ref often


let check v =
  assert ((1 <= v) && (v <= 6))


let with_level v f =
  check v ;
  let prev = !current in
    current := v ;
    let res = f () in
      current := prev ;
      res


let pf v ch=
  (** will allocate and compute string even when nothing is being
    printed, but allows pretty code *)
  let output_and_flush ch s =
    output_string ch s;
    flush ch
  in
  check v;
  if (v <= !current) then begin
    Printf.ksprintf (output_and_flush ch)
  end else
    Printf.ksprintf ignore

let echo v ch =
  check v;
  Printf.ksprintf (fun s ->
		     if (v <= !current) then output_string stdout s;
		     output_string ch s)

let pr v =
  pf v stdout

let pe v =
  pf v stderr


let force v thunk =
  (** for when evaluating the arguments of [pf] might take significant
    time.  "lazy" is a little shorter to write than "fun ->" *)
  check v;
  if (v <= !current)
  then Lazy.force thunk


let level v =
  (v <= !current)


let set_default v =
  check v;
  Wrutils.pe "%!Setting default verbosity to %d.\n%!" v;
  current := v


(* EOF *)
