(* $Id: matrix.ml,v 1.1 2003/11/07 20:56:24 ruml Exp $

   functions on basic OCaml matrices (arrays of arrays of floats).

   The inner arrays are the rows, so follows "row-major" semantics:
   a.(row).(col)
*)


(************* iteration and mapping *************)


let iter f mat =
  Array.iter (Array.iter f) mat


let iterij f mat =
  Array.iteri (fun i row ->
		 (Array.iteri (fun j x -> f i j x)
		    row))
    mat


let map f mat =
  Array.map (fun r -> Array.map f r) mat


(************** querying *****************)


let num_rows mat =
  Array.length mat

let num_cols mat =
  (** assumes rectangular *)
  Array.length mat.(0)


let row_sums mat =
  Array.map Vector.sum mat


let col_sum m col_index =
  Vector.sum_using (fun row -> row.(col_index)) m

let col_sums mat =
  Array.init (num_cols mat)
    (col_sum mat)


let rectangular_p m =
  let cols = num_cols m in
    Wrarray.for_all (fun a -> (Array.length a) = cols)
      m


let col m i =
  (** get the [i]th column of matrix [m]. *)
  let n = num_rows m in
  let r = Array.make n 0. in
    for j = 0 to n - 1 do r.(j) <- m.(j).(i) done;
    r

let row m i = m.(i)
  (** get the [i]th row of matrix [m]. *)


(***************** modification *****************)


let set_diagonal m v =
  for i = 0 to (num_rows m) - 1 do
    m.(i).(i) <- v
  done


(************** transformation ********************)

let copy mat =
  Array.map Array.copy mat


let from_ints int_mat =
  map float int_mat


let transpose m =
  let a = num_rows m
  and b = num_cols m in
    Array.init b (fun i ->
		    Array.init a (fun j -> m.(j).(i)))


let absolute m =
  map abs_float m


let extend m rows cols init =
  (** extends [m] by [rows] and [cols], reusing elements of [m] where
      possible and usomg [init] elsewhere *)
  let old_rows = num_rows m
  and old_cols = num_cols m in
  let new_cols = old_cols + cols in
    Array.init (old_rows + rows)
      (fun row ->
	 if row >= old_rows
	 then Array.make new_cols init
	 else Wrarray.extend m.(row) cols init)

let add a b =
  (** componont-wise addition. *)
  Wrarray.map2 (fun x y -> Vector.add x y) a b


let mulf_scalar m scalar =
  (** [mulf_scalar m scalar] mulitply the matrix by a scalar value. *)
  Array.map (fun v -> Vector.multiply_by v scalar) m


let mul_matrix_vec add_ident mul_fun add_fun mat vec =
  (** [mul_matrix_vec add_fun mul_fun add_fun mat vec] multiplies
      [mat] by [vec] given the multiplication function [mul_fun] the
      addition function [add_fun] and the additive identity value
      [add_ident].  Instead of using this function, you should
      probably use the convenient wrappers for integers and floats:
      [muli] [mulf]. *)
  assert ((Array.length mat) > 0);
  let rows = Array.length mat
  and cols = Array.length mat.(0) in
    if (Array.length vec) <> cols
    then invalid_arg (Printf.sprintf
			"Matrix.mul: vector has %d elements, %d expected"
			(Array.length vec)
			cols)
    else
      let result = Array.make rows add_ident in
	for r = 0 to rows - 1 do
	  let sum = ref add_ident in
	    for c = 0 to cols - 1 do
	      sum := add_fun !sum (mul_fun mat.(r).(c) vec.(c))
	    done;
	    result.(r) <- !sum;
	done;
	result

let muli_vec = mul_matrix_vec 0 ( * ) (+)
  (** [muli_vec mat vec] multiplies the integer matrix [mat] by the
      integer vector [vec]. *)

let mulf_vec = mul_matrix_vec 0.0 ( *. ) (+.)
  (** [mulf_vec mat vec] multiplies the float matrix [mat] by the
      float vector [vec]. *)


let mulf a b =
  (** [mulf a b] multiplies matrix [a] with [b]... assumes the
      matrixes are in row major order. *)
    if not ((num_cols a) = (num_rows b))
    then invalid_arg "Matrix.mulf: matrices are not mulitpliable";
    let r = Array.init (num_rows a) (fun _ -> Array.make (num_cols b) 0.) in
      for i = 0 to (num_cols b) - 1 do
	let colb = col b i in
	for j = 0 to (num_rows a) - 1 do
	  let rowa = row a j in
	    r.(i).(j) <- Vector.dot rowa colb;
	done
      done;
      r


let rec determinant m =
  (** [determinant m] computes the determinant of a square matrix. *)
  if not (rectangular_p m)
  then invalid_arg "Matrix.determinant: not a square matrix";
  let n = num_rows m in
    if n = 1
    then m.(0).(0)
    else begin
      let r = ref 0. in
	for i = 0 to n - 1 do
	  let min = minor m 0 i in
	    r := !r +. ((~-.1.) ** (float i)) *. m.(0).(i) *. min;
	done;
	!r
    end


and minor m i j =
  (** [minor m i j] computes the [i] (row) [j] (col) minor of
      [m].

      Note: matrices are indexed from zero here, not from 1 as math
      would suggest. *)
  if not (rectangular_p m)
  then invalid_arg "Matrix.minor: not a square matrix";
  let n = num_rows m in
  let r = Array.make_matrix (n - 1) (n - 1) 0.
  and rrow = ref 0
  and rcol = ref 0 in
    for k = 0 to n - 1 do
      if k <> i
      then begin
	rcol := 0;
	for l = 0 to n - 1 do
	  if l <> j
	  then begin
	    r.(!rrow).(!rcol) <- m.(k).(l);
	    incr rcol
	  end;
	done;
	incr rrow
      end;
    done;
    determinant r


let cofactor m i j =
  (** [cofactor m i j] computes the [i] [j] cofactor of [m].

      Note: this assumes that matrix indeices start at zero not one. *)
  let min = minor m i j in
    if Math.even_p (i + j) then min else ~-. min


let invert m =
  (** [invert m] inverts the matrix [m].  This is probably pretty slow. *)
  if not (rectangular_p m)
  then invalid_arg "Matrix.invert: not a square matrix";
  if (num_rows m) = 1
  then
    (* special case, can't get cofactor matrix of a one element matrix so
       lets just invert it here. *)
    [| [| 1. /. m.(0).(0) |] |]
  else
    let det = determinant m in
      if det = 0.
      then invalid_arg "Matrix.invert: determinant is zero";
      let n = num_rows m in
      let inv = Array.make_matrix n n 0. in
	for i = 0 to n - 1 do
	  for j = 0 to n - 1 do
	    inv.(j).(i) <- (cofactor m i j) /. det
	  done
	done;
	inv


(************* I/O ******************)


let read_matrix_body ic cols rows =
  Array.init rows (fun i -> Vector.read ic cols)


let read_matrix inch =
  let rows = Wrio.input_int inch in
  let cols = Wrio.input_int inch in
    read_matrix_body inch cols rows


let write_matrix_body outch m =
  Array.iter (fun a ->
		Vector.write outch a ;
		Wrutils.newline outch)
    m


let write_matrix outch m =
  let rows = num_rows m
  and cols = num_cols m in
    if not (rectangular_p m) then failwith "matrix not rectangular";
    Verb.pf Verb.always outch "%d %d\n" rows cols;
    write_matrix_body outch m


(* EOF *)
