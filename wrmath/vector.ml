(* $Id: vector.ml,v 1.1 2003/11/07 20:56:04 ruml Exp ruml $

   arrays of floats
*)


let from_ints a =
  Array.map float a


(*********** querying **********)


let min_and_max a =
  let l = Array.length a in
    if l > 0 then
      let min = ref (a.(0) : float)
      and max = ref a.(0) in
	for i = 1 to l-1 do
	  let e = a.(i) in
	    if e < !min then
	      min := e
	    else if e > !max then
	      max := e
	done;
	!min, !max
    else
      invalid_arg "Vector.min_and_max: given empty array"


let sum_using key a =
  Array.fold_left (fun a x -> a +. (key x)) 0. a


let sum a =
  let sum = ref 0. in
    for i = 0 to (Array.length a)-1 do
      sum := !sum +. a.(i)
    done;
    !sum


let avg a =
  (sum a) /. float_of_int (Array.length a)

let dot a b =
  let n = Array.length a in
    if ((Array.length b) <> n) then invalid_arg "iter2: different lengths";
    let sum = ref 0. in
      for i = 0 to (n-1) do
	sum := !sum +. (a.(i) *. b.(i))
      done;
      !sum


let norm v =
  let n = Array.length v
  and sum = ref 0. in
    for i = 0 to (n-1) do
      sum := !sum +. (v.(i) *. v.(i))
    done;
    sqrt !sum


let rmsd a b =
  (** root mean squared deviation *)
  let sum = ref 0. in
    Wrarray.iter2 (fun x y ->
		     sum := !sum +. Math.square (x -. y))
      a b;
    sqrt (!sum /. (float (Array.length a)))


let euclidean a b =
  let sum = ref 0. in
    Wrarray.iter2 (fun x y ->
		     sum := !sum +. Math.square (x -. y))
      a b;
    sqrt !sum


let cosine a b =
  (** often used as a similarity measure.  1 for identical.  zero
    vector gives 0 *)
  let denom = (norm a) *. (norm b) in
    if denom = 0. then
      0.
    else
      (dot a b) /. denom


(********* transforming **********)

let add a b =
  (** [a] + [b] componentwise *)
  Wrarray.map2 (+.) a b

let subtract a b =
  (** [a] - [b] componentwise *)
  Wrarray.map2 (-.) a b


let multiply_by a scalar =
  (** nondestructive *)
  Array.map (fun x -> x *. scalar) a


let project pt dir scalar =
  (** projects [pt] in [dir] by [scalar].  element-wise [dir]*[scalar]
    + [pt].  Nondestructive. *)
  Wrarray.map2 (fun p d -> (d *. scalar) +. p)
    pt dir


let cumulative a =
  let len = Array.length a in
  let b = Array.make len 0.
  and max = len - 1 in
    b.(max) <- a.(max);
    for i = max - 1 downto 0 do
      b.(i) <- b.(i+1) +. a.(i)
    done;
    b


(********* I/O **********)


let read ic n =
  (** returns an array of [n] floats *)
  Array.init n (fun _ -> Wrio.input_float ic)


let read_from_line ic =
  (** parses the next line into an array of floats *)
  Array.of_list (Wrstr.parse_floats (input_line ic))


let write oc v =
  (** writes all numbers on same line, separated by spaces *)
  Wrarray.fprint_array oc string_of_float " " v


let write_line oc v =
  (** writes all numbers on same line, then prints newline *)
  write oc v;
  Wrutils.newline oc


let write_nl oc a =
  (** one number per line *)
  Wrarray.fprint_array oc string_of_float "\n" a;
  Wrutils.newline oc


let print v =
  write stdout v


(* EOF *)
