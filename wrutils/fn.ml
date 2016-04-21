(** Some simple functions.

    @author eaburns
    @since 2010-01-05
*)

(** Function composition: returns a function of one argument that
    calls [f] on result of calling [g] on the argument *)
let compose f g x = f (g x)

(** Sequencing operator. *)
let ( |> ) x f = f x

let identity a = a


let constantly0 x = (fun () -> x)


let constantly1 x = (fun _ -> x)


let constantly2 x = (fun _ _ -> x)


let neg1 f x = not (f x)

let neg2 f x y = not (f x y)

let no_op1 _ = ()

let no_op2 _ _ = ()

let no_op3 _ _ _ = ()

let no_op4 _ _ _ _ = ()

let no_op5 _ _ _ _ _ = ()

let no_op6 _ _ _ _ _ _ = ()


let gather2 a b =
  (** returns the two arguments as a pair.  for scanf, for instance? *)
  a, b

let gather3 a b c =
  a, b, c


let int_compare (a:int) (b:int) =
  if a < b then -1
  else if a > b then 1
  else 0
