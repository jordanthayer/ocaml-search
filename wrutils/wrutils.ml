(* $Id: utils.ml,v 1.2 2003/09/27 01:11:58 ruml Exp ruml $

   misc utility code
*)


(***** quick debugging utilities ******)


let write_this () = prerr_string "WRITE_THIS\n"


let not_impl () = failwith "not yet implemented"


let pr = Printf.printf
let pe = Printf.eprintf

let pf = Printf.fprintf
let newline ch = output_char ch '\n'

let fs str =
  (** flushes a string to stdout *)
  flush_all ();
  print_string str;
  flush_all ()


let failf fmt =
  (** give format string and args, raises failure with formatted string *)
  Printf.kprintf (fun s -> failwith s) fmt


exception Nicefail of string


let nicefailwith s =
  raise (Nicefail s)


let catch_nicefail f =
  try
    f ()
  with Nicefail s ->
    flush_all ();
    pe "\nSorry, the program ran into difficulty:\n%s\n" s;
    exit 1


(****** useful misc functions *******)


let str = Printf.sprintf
  (** given format string and args, returns string *)


(*
 * Some functions that were previously here were moved to fn.ml
 *)


let push e l_ref =
  l_ref := e::!l_ref

let push_new e l_ref =
  if not (List.mem e !l_ref) then
    l_ref := e::!l_ref


let swap a b =
  let t = !a in
    a := !b;
    b := t


let arg_max x y f = if (f x) > (f y) then x else y

let arg_min x y f = if (f x) < (f y) then x else y



(***** flow of control ******)


let unwind_protect f f_arg clean_f clean_f_arg =
  (** evaluates [f] on [f_arg], ensuring that [clean_f] is called on
    [clean_f_arg] afterwards, even if [f] raises an exception *)
  let res = (try
	       f f_arg
	     with x -> clean_f clean_f_arg; raise x)
  in
    (* execution won't reach here if we processed an exception above *)
    clean_f clean_f_arg; res


let eager_or thunk1 thunk2 =
  (* always forces evaluation of both branches.  args must be wrapped by
     lazy.  This is necessary to guarantee order of evaluation. *)
  let res1 = Lazy.force thunk1 in
  let res2 = Lazy.force thunk2 in
    res1 || res2


(************* iteration **************)


let rec eval_until f pred =
  (** evaluates [f] until its result makes [pred] return true.  the
    result is then returned. *)
  let res = f () in
    if pred res
    then res
    else eval_until f pred


let iter_until f pred =
  ignore (eval_until f pred)


let rec do_until f l =
  (** iterates [f] over [l] until it returns non-None *)
  match l with
      [] -> None
    | x::rest ->
	match f x with
	    None -> do_until f rest
	  | result -> result


let rec for_while f init_i =
  (** [f] takes increasing integers and returns an option value.  If
    non-None, iteration ceases and the value is returned. *)
  match f init_i with
    None -> for_while f (init_i + 1)
  | Some x -> x


let ntimes f n =
  (** calls [f] on unit [n] times.  returns unit *)
  for i = 1 to n do
    f ()
  done


(************* mapping **************)


let rec fold_while f initial =
  (** [f] is given [initial] or the fold value and returns whether or
    not to continue and a new value for returning or more folding *)
  let continue, next = f initial in
    if continue
    then fold_while f next
    else next


let rec foldi_while f init_i init_val =
  (** [f] takes [i] (an incrementing integer) and a value and returns
    whether or not to continue and a new value for returning or for more
    folding *)
  let continue, next = f init_i init_val in
    if continue
    then foldi_while f (init_i + 1) next
    else next


let map_while f =
  (** evaluates [f] on unit until it returns None.  returns a list of the
    previous non-None results of [f]. *)
  (* could have been built on [fold_while] *)
  let rec try_f prev =
    match f () with
      None -> List.rev prev
    | Some e -> try_f (e::prev)
  in
    try_f []


(* used to be called generate_n *)
let map_n f n =
  (** returns a list of the results of calling [f] on integers 0
    through [n] (inclusive) *)
  let rec rest i =
    if i > n
    then []
    else
      let v = f i in
	v::(rest (i+1))
  in
    rest 0


let iter_for low high incr f =
  (** calls f on values starting at low, increasing by incr, until past
    high *)
  let i = ref low in
    while !i <= high do
      f !i;
      i := !i + incr
    done


let iter_for_f low high incr f =
  (** calls f on floating point values starting at low, increasing by incr,
    until past high *)
  let i = ref low in
    while !i <= high do
      f !i;
      i := !i +. incr
    done


let rec map_for acc low high incr f =
  (** calls f on values starting at low, increasing by incr, until
      past high *)
  if low <= high then
    map_for ((f low)::acc) (low + incr) high incr f
  else
    acc


let rec map_for_f acc low high incr f =
  (** calls f on floating point values starting at low, increasing by incr,
      until past high *)
  if low <= high then
    map_for_f ((f low)::acc) (low +. incr) high incr f
  else
    acc


let rec map_ntimes f n =
  (** returns a list of the results of calling [f] on unit [n] times *)
  if n < 0 then
    invalid_arg "map_ntimes"
  else if n = 0 then
    []
  else
    let res = f () in
      res::(map_ntimes f (n - 1))


let map_n_opt f n =
  (** returns a list of the non-None results of calling [f] on
    integers 0 through [n] (inclusive, just like a [for] loop) *)
  let rec rest i =
    if i > n
    then []
    else
      match f i with
	None -> rest (i + 1)
      | Some x -> x::(rest (i + 1))
  in
    rest 0


(* EOF *)
