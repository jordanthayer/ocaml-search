(* A min-heap of floats.  This structure may work better than a binary
   heap if there are many insertions of elements that have high key
   values and will never be removed. *)

type 'a ent = {
  data : 'a;
  mutable vl : float;
  mutable ind : int;
}

type 'a elms = {
  mutable ary : 'a ent array;
  mutable fill : int;
  mutable heap : bool;
}

let round f = 
  if(f >= 0.0) then int_of_float f
  else (int_of_float (f)) - 1


let no_pos = ~-1

let parent i =
  (i - 1) / 2

let left i =
  2 * i + 1

let right i =
  2 * i + 2

let swap ary i j =
  let ai = ary.(i) and aj = ary.(j) in
  ary.(i) <- aj;
  ary.(j) <- ai;
  ai.ind <- j;
  aj.ind <- i

let pred tie a b =
  if a.vl = b.vl then tie a b else a.vl < b.vl

let rec push_down h tie i =
  let ary = h.ary and fill = h.fill in
  let l = left i and r = right i in
  let sml = if l < fill && pred tie ary.(l) ary.(i) then l else i in
  let sml = if r < fill && pred tie ary.(r) ary.(sml) then r else sml in
    if sml <> i then begin
    swap ary sml i;
    push_down h tie sml
  end

(** Initialize a heap in linear time. *)
let heapify h tie =
  for i = h.fill / 2 downto 0 do
    push_down h tie i
  done;
  h.heap <- true

let rec pull_up h tie i =
  let ary = h.ary in
  let p = parent i in
  if p > 0 && pred tie ary.(i) ary.(p) then begin
    swap ary p i;
    pull_up h tie p
  end

let consider_growing h elm =
  let init_size = 100 in
  let ary = h.ary and fill = h.fill in
  let size = Array.length ary in
  if size = 0 then
    Array.create init_size elm
  else if fill + 1 >= size then
    Array.init (size * 2) (fun i -> if i < fill then ary.(i) else elm)
  else
    ary

(** Inserts an element into the array and then pulls it up in the
    heap. *)
let heap_push h tie elm =
  let fill = h.fill in
  let ary = consider_growing h elm in
  ary.(fill) <- elm;
  elm.ind <- fill;
  pull_up h tie fill;
  h.fill <- fill + 1

exception Empty

(** Pops the head element from the heap and returns it. *)
let heap_pop h tie =
  let fill = h.fill and ary = h.ary in
  if fill = 0 then raise Empty;
  let e = ary.(0) in
  e.ind <- no_pos;
  ary.(0) <- ary.(fill);
  ary.(0).ind <- 0;
  push_down h tie 0;
  h.fill <- fill - 1;
  e

(** Inserts the element into the array but does not maintain the heap
    property.  This is amortized constant time. *)
let ary_push h elm =
  let ary = consider_growing h elm in
  ary.(h.fill) <- elm;
  h.fill <- h.fill + 1;
  h.heap <- false

let push h tie elm =
  if h.heap then
    heap_push h tie elm
  else
    ary_push h elm

let pop h tie elm =
  if not h.heap then heapify h tie;
  heap_pop h tie

let is_empty h =
  h.fill = 0

type 'a t = {
  mutable start : float;
  mutable width : float;
  mutable bins : 'a elms array;
  mutable smallest_bin : int;
  tie : 'a -> 'a -> bool;
  prototype: 'a;
}



let make_elm () = 
{
  ary = [||];
  fill = 0;
  heap = false;
}


let grow_low h new_min = 
  (*grows the heap in the downward direction*)
  let current_min = h.start in
    assert(current_min > new_min);
    let new_bucket_count = 
      int_of_float((current_min -. new_min) /. h.width) + 1 in
    let new_bins = 
      Array.init ((Array.length h.bins)+new_bucket_count) 
	(fun _ -> make_elm ()) in 
      Printf.fprintf stderr "%d %d %d\n"
	new_bucket_count 
	(Array.length h.bins) 
	(Array.length new_bins);
      Array.blit h.bins new_bucket_count new_bins new_bucket_count
	(Array.length h.bins);
      h.bins <- new_bins;
      h.start <- (h.start -. (h.width *. (float_of_int new_bucket_count)));
      h.smallest_bin <- new_bucket_count


let grow_high h new_max = ()
  (*grows the heap in the upward direction*)


let create ?(initial_size = 10) ?(update_function = fun a b -> ())
    tie initial_val delta prototype =
{
  start = initial_val;
  width = delta;
  bins = Array.init initial_size (fun _ -> make_elm ());
  smallest_bin = max_int;
  tie = tie;
  prototype = prototype

}

let insert h item= failwith "not implemented"
let pop h = failwith "not implemented"
let peek_first h = failwith "not implemented"

let count h = failwith "not implemented"
let empty_p h = failwith "not implemented"
let remove_at h index value = failwith "not implemented"

let extract_first h = failwith "not implemented"


