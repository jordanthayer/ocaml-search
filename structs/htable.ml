(** Same as the OCaml Hashtbl module, except now you can pass in your
    own hash function and you don't have to use the functorial
    interface.
*)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  {
    hash : 'a -> int;			    (* hash function *)
    equal : 'a -> 'a -> bool;		    (* equality test *)
    mutable size: int;			    (* number of elements *)
    mutable data: ('a, 'b) bucketlist array; (* the buckets *)
    mutable bucket_sizes : int array;
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

let create hashfun equalfun initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
    { hash = hashfun;
      equal = equalfun;
      size = 0;
      data = Array.make s Empty;
      bucket_sizes = Array.make s 0;
    }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty;
    h.bucket_sizes.(i) <- 0;
  done;
  h.size <- 0

let copy h =
  { h with
      size = h.size;
      data = Array.copy h.data;
      bucket_sizes = Array.copy h.bucket_sizes;
  }

let length h = h.size

let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let bucket_sizes' = Array.create nsize 0 in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i);
      bucket_sizes'.(i) <- bucket_sizes'.(i) + 1;
    done;
    tbl.data <- ndata;
    tbl.bucket_sizes <- bucket_sizes';
  end

let add h key info =
  let i = (h.hash key) mod (Array.length h.data) in
  let bucket = Cons(key, info, h.data.(i)) in
    h.data.(i) <- bucket;
    h.bucket_sizes.(i) <- h.bucket_sizes.(i) + 1;
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize h.hash h

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if h.equal k key
        then begin h.size <- pred h.size; next end
        else Cons(k, i, remove_bucket next) in
  let i = (h.hash key) mod (Array.length h.data) in
    h.data.(i) <- remove_bucket h.data.(i);
    h.bucket_sizes.(i) <- h.bucket_sizes.(i) - 1

let rec find_rec h key = function
    Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if h.equal key k then d else find_rec h key rest

let find h key =
  match h.data.((h.hash key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if h.equal key k1 then d1 else
      match rest1 with
        Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if h.equal key k2 then d2 else
          match rest2 with
            Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if h.equal key k3 then d3 else find_rec h key rest3

let find_all h key =
  let rec find_in_bucket = function
    Empty ->
      []
  | Cons(k, d, rest) ->
      if h.equal k key
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.((h.hash key) mod (Array.length h.data))


let find_or_default ht key def =
  (** looks up [key] in [ht], using [def] as value on failure *)
  try
    find ht key
  with Not_found -> def

let replace h key info =
  let rec replace_bucket = function
      Empty ->
        raise Not_found
    | Cons(k, i, next) ->
        if h.equal k key
        then Cons(k, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = (h.hash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.bucket_sizes.(i) <- h.bucket_sizes.(i) + 1;
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize h.hash h

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons(k, d, rest) ->
      h.equal k key || mem_in_bucket rest in
  mem_in_bucket h.data.((h.hash key) mod (Array.length h.data))

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

let map f ht =
  (** a list of the results of evaluating [f] on the keys and values *)
  fold (fun k v accum ->
	  (f k v)::accum)
    ht []


let bucket_stats h =
  let sizes = Array.copy h.bucket_sizes in
    Array.sort Math.icompare sizes;
    let n = Array.length sizes in
    let med =
      if Math.divisible n 2 then
	(sizes.(n / 2 - 1) + sizes.(n / 2)) / 2
      else
	sizes.(n / 2)
    in
      h.size, n, sizes.(0), med, sizes.(n - 1)
