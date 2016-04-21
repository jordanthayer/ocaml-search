(** floating point references

    @author eaburns
    @since 2010-07-25
*)

type t = { mutable data : float }

type float_ref = t

let float_ref f = { data = f }

let (<--) r f = r.data <- f

let (!!) r = r.data


let incf t =
  t.data <- t.data +. 1.

let addf t v =
  t.data <- t.data +. v
