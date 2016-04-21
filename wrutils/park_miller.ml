(** Park and Miller's Minimal Standard Random Number Generator
    Adapted from Wheeler's lisp implementation and this C implementation
    http://www.firstpr.com.au/dsp/rand31/rand31-park-miller-carta.cc.txt *)

(* Totally not thread safe *)

let state = ref 1.
and constant_a = 16807.
and constant_m = 2147483647.


let set_state v = state := v

let get_state () = !state

let next () =
  state := Math.fmod (constant_a *. !state) constant_m;
  (!state /. constant_m)

let next_from_state s =
  (* clobbers current state *)
  state := s;
  next()

let init i =
  if i = 0 then state := 1.
  else state := float_of_int i

(* EOF *)
