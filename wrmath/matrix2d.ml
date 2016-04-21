(** A more efficient implementation of 2D matricies.

    The routines are written in an attempt to reduce the amount of
    floating point boxing that is needed.

    Some functions perform inplace modification and some result in a
    new matrix.  The function names should be verbs if they are doing
    inplace modification or nouns if they result in a new matrix.

    TODO:

    * determinant using LU decomposition should be faster than the
    current routine for matricies greater than 3x3.

    * inversion is still slower than lapack.

    @author eaburns
    @since 2010-02-09
*)

open Printf

type t = {
  rows : int;
  cols : int;
  data : float array;
}


let base ~cols ~row = row * cols
    (** [base ~cols ~row] gets the base index of a row. *)


let index ~cols ~row ~col = (base ~cols ~row) + col
  (** [index ~cols ~row ~col] gets the data intex for the given
      position. *)


let init ~rows ~cols f =
  (** [init ~rows ~cols f] initializes a new matrix of [rows]x[cols]
      calling [f] on each element. *)
  let init_fun rows cols f ind =
    (* calls the user's initialization function with the row and
       column instead of the data index. *)
    let row = ind / cols
    and col = ind mod cols
    in f row col
  in
    {
      rows = rows;
      cols = cols;
      data = Array.init (rows * cols) (init_fun rows cols f)
    }


let make ~rows ~cols vl =
  (** [make ~rows ~cols vl] makes a new [rows]x[cols] matrix initalized
      to [vl] at each element. *)
  {
    rows = rows;
    cols = cols;
    data = Array.make (rows * cols) vl
  }


let of_vector v =
  (** [of_vector v] converts a vector (float array) into a 1 column
      matrix.

      Note: This does not copy the vector [v], it simply refers to the
      input vector.  This makes it faster if a copy is not desired.
      Changes to [v] will be reflected in the matrix returned from
      this function.  If this is not desired, then you should make a
      copy of the result. *)
  let n = Array.length v in
    {
      rows = n;
      cols = 1;
      data = v;
    }


let of_array2d_copy a =
  (** [of_array2d_copy a] makes a matrix that is a copy of the 2d
      OCaml array [a]. *)
  let rows = Array.length a
  and cols = Array.length a.(0)
  in init ~rows ~cols (fun r c -> a.(r).(c))


let copy t = { t with data = Array.copy t.data }
  (** [copy t] gets a fresh copy of the matrix [t].  *)


let identity ~rows ~cols =
  (** [identity ~rows ~cols] make a [rows]x[cols] indentity matrix. *)
  let init_identity r c = if r = c then 1.0 else 0.
  in init ~rows ~cols init_identity


let at t ~row ~col = t.data.(index ~cols:t.cols ~row ~col)
  (** [at t ~row ~col] gets the value at the given row and column. *)


let set t ~row ~col vl = t.data.(index ~cols:t.cols ~row ~col) <- vl
  (** [set t ~row ~col] set the value at the given row and column. *)


let to_string t =
  (** [to_string t] gets a string representation of the matrix. *)
  let base = base ~cols:t.cols in
  let data = t.data in
  let buf = Buffer.create 100 in
    for i = 0 to t.rows - 1 do
      let b = base ~row:i in
	for j = 0 to t.cols - 1 do
	  Buffer.add_string buf (sprintf "%4g " (data.(b + j)))
	done;
	Buffer.add_string buf "\n";
    done;
    Buffer.contents buf


let transposed t =
  (** [transpose t] gets the transpose of matrix [t]. *)
  let tt = make ~rows:t.cols ~cols:t.rows 0. in
  let ttdata = tt.data
  and tdata = t.data
  and ttbase = base ~cols:tt.cols
  and tind = index ~cols:t.cols in
    for i = 0 to t.cols - 1 do
      let b = ttbase i in
      for j = 0 to t.rows - 1 do
	ttdata.(b + j) <- tdata.(tind j i)
      done
    done;
    tt


let product a b =
  (** [product a b] gets the product of the two matricies [a] and
      [b]. *)
  let abase = base ~cols:a.cols
  and bind = index ~cols:b.cols
  and adata = a.data
  and bdata = b.data in
  let n = a.cols in
    if b.rows <> n then invalid_arg "product: matricies not multiplyable";
    let prod = make ~rows:a.rows ~cols:b.cols 0. in
    let data = prod.data in
      for i = 0 to a.rows - 1 do
	let ab = abase i in
	  for j = 0 to b.cols - 1 do
	    let ind = index ~cols:b.cols ~row:i ~col:j in
	      for r = 0 to n - 1 do
		data.(ind) <- (data.(ind)
			       +. (adata.(ab + r)) *. (bdata.(bind r j)))
	      done
	  done
      done;
      prod


let sum a b =
  (** [sum a b] gets a matrix that is the element-by-element sum of
      [a] and [b]. *)
  if a.rows <> b.rows || a.cols <> b.cols
  then invalid_arg "sum: matricies do not have the same dimensions";
  let s = copy a in
  let sdata = s.data
  and bdata = b.data in
    for i = 0 to a.rows - 1 do
      let row_base = base ~cols:b.cols ~row:i in
	for j = 0 to a.cols - 1 do
	  let index = row_base + j in
	    sdata.(index) <- sdata.(index) +. bdata.(index)
	done
    done;
    s


let diff a b =
  (** [diff a b] gets a matrix that is the element-by-element
      difference of [a] and [b]. *)
  if a.rows <> b.rows || a.cols <> b.cols
  then invalid_arg "diff: matricies do not have the same dimensions";
  let s = copy a in
  let sdata = s.data
  and bdata = b.data in
    for i = 0 to a.rows - 1 do
      let row_base = base ~cols:b.cols ~row:i in
	for j = 0 to a.cols - 1 do
	  let index = row_base + j in
	    sdata.(index) <- sdata.(index) -. bdata.(index)
	done
    done;
    s


let minor t ~row ~col =
  (** [minor t ~row ~col] computes the [row], [col] minor of [t]. *)
  let m = make ~rows:(t.rows - 1) ~cols:(t.cols - 1) 0. in
  let tdata = t.data
  and mdata = m.data in
    for i = 0 to m.rows - 1 do
      let b = base m.cols i in
	for j = 0 to m.cols - 1 do
	  let r = if i < row then i else i + 1
	  and c = if j < col then j else j + 1 in
	    mdata.(b + j) <- tdata.(index t.cols r c)
	done
    done;
    m


let rec determinant t =
  (** [determinant t] gets the determinant of the matrix.

      For a matrix that is larger than 3x3, this is a bit slow and instead
      we should implement LU decomposition which I don't really
      understand right now. *)
  let rows = t.rows and cols = t.cols in
  let data = t.data in
    if rows <> cols then invalid_arg "determinant: matrix is not square";
    if rows = 1
    then data.(0)
    else begin
      let r = ref 0. in
	for i = 0 to rows - 1 do
	  let m = determinant (minor t 0 i) in
	    r := !r +. ((~-.1.) ** (float i)) *. data.(i) *. m;
	done;
	!r
    end


let slow_inverse t =
  (** [slow_inverse t] computes the inverse of [t].

      This may be a slower than other implementations, but it is easy
      to code. *)
  if t.rows <> t.cols then invalid_arg "invert: matrix is not invertable";
  let n = t.rows in
  let inv = make ~rows:n ~cols:n 0. in
  let idata = inv.data in
    if n = 1
    then inv.data.(0) <- 1. /. t.data.(0)
    else begin
      let det = determinant t in
	if det = 0. then invalid_arg "invert: matrix determinant is zero";
	for j = 0 to n - 1 do
	  let b = base ~cols:inv.cols ~row:j in
	    for i = 0 to n - 1 do
	      let ind = b + i in
		idata.(ind) <- determinant (minor t i j);
		if (i + j) mod 2 <> 0 then idata.(ind) <- ~-.(idata.(ind));
		idata.(ind) <- idata.(ind) /. det;
	    done
	done
    end;
    inv


let lu_decomposition t =
  (** [lu_decomposition t] performs an lu decomposition of the given
      matrix. *)
  let rows = t.rows and cols = t.cols in
  let upper = make ~rows ~cols 0.
  and lower = make ~rows ~cols 0. in
  let u = upper.data and l = lower.data and a = Array.copy t.data in
    for k = 0 to rows - 1 do
      let k_base = base ~cols ~row:k in
      let ukk = a.(k_base + k) in
	u.(k_base + k) <- ukk;
	l.(k_base + k) <- 1.;
	for i = k + 1 to rows - 1 do
	  let i_base = base ~cols ~row:i in
	    l.(i_base + k) <- a.(i_base + k) /. ukk;
	    u.(k_base + i) <- a.(k_base + i);
	done;
	for i = k + 1 to rows - 1 do
	  let i_base = base ~cols ~row:i in
	    for j = k + 1 to rows - 1 do
	      a.(i_base + j) <- a.(i_base + j) -. (l.(i_base + k)
						   *. u.(k_base + j));
	    done;
	done;
    done;
    lower, upper



let lup_decomposition t ?(pi=Array.init t.rows (fun i -> i)) ()  =
  (** [lup_decomposition t ?pi ()] computes the LUP decomposition of
      the matrx [t].  This uses an in-place optimization that is
      destructive on the input matrix.  The result is that [t] is
      modified so its upper half is U and its lower half is L.  P is
      given as a permutation array.

      This is from "Introduction to Algorithms 2nd Edition", Corman,
      Leiserson, Rivest and Stein.  *)
  let a = t.data and rows = t.rows and cols = t.cols in
  let p = [| 0. |] and k' = ref 0 in
    for k = 0 to rows - 1 do
      p.(0) <- 0.;
      for i = k to rows - 1 do
	let i_base = base ~cols ~row:i in
	let abs_aik = abs_float (a.(i_base + k)) in
	  if abs_aik > p.(0)
	  then begin
	    p.(0) <- abs_aik;
	    k' := i;
	  end
      done;
      if p.(0) = 0. then invalid_arg "lup_decomposition: singular matrix";
      (let tmp = pi.(k) in pi.(k) <- pi.(!k'); pi.(!k') <- tmp);
      let k_base = base ~cols ~row:k and k'_base = base ~cols ~row:(!k') in
	for i = 0 to rows - 1 do
	  let tmp = a.(k_base + i) in
	    a.(k_base + i) <- a.(k'_base + i);
	    a.(k'_base + i) <- tmp;
	done;
	for i = k + 1 to rows - 1 do
	  let i_base = base ~cols ~row:i in
	    a.(i_base + k) <- a.(i_base + k) /. a.(k_base + k);
	    for j = k + 1 to rows - 1 do
	      a.(i_base + j) <- a.(i_base + j) -. (a.(i_base + k)
						   *. a.(k_base + j));
	    done
	done;
    done;
    t, pi


let lup_solve
    lu ?(x=Array.create lu.rows 0.) ?(y=Array.create lu.rows 0.) pi b =
  (**[lup_solve lu ?x ?y pi b] solves a set of linear equations using
     LUP decomposition.  [lu] is a single matrix that contains L as
     its lower half and U as its upper.  [pi] is P in per-mutation
     form. *)
  let rows = lu.rows and cols = lu.cols in
  let l = lu.data and u = lu.data in
  let sum = [| 0. |] in
    for i = 0 to rows - 1 do
      let i_base = base ~cols ~row:i in
	sum.(0) <- 0.;
	for j = 0 to i - 1 do
	  sum.(0) <- sum.(0) +. l.(i_base + j) *. y.(j)
	done;
	y.(i) <- b.(pi.(i)) -. sum.(0);
    done;
    for i = rows - 1 downto 0 do
      let i_base = base ~cols ~row:i in
	sum.(0) <- 0.;
	for j = i + 1 to rows - 1 do
	  sum.(0) <- sum.(0) +. u.(i_base + j) *. x.(j);
	done;
	x.(i) <- (y.(i) -. sum.(0)) /. u.(i_base + i);
    done;
    x


let solve a b =
  (** [solve a b] solves for x in the equation [a x = b]. *)
  let lu, pi = lup_decomposition (copy a) () in
  let x = lup_solve lu pi b.data in
    { rows = b.rows; cols = b.cols; data = x }


let lup_inverse a =
  (** [lup_inverse a] inverts a matrix using LUP decomposition.  This
      attempts to minimize allocations by re-using temporary matricies
      etc. *)
  let rows = a.rows and cols = a.cols in
  let n = rows * cols in
  let inv = make ~rows ~cols 0. in
  let inv_dat = inv.data in
  let id_col = Array.create rows 0. in
  let x = Array.create rows 0. and y = Array.create rows 0. in
  let pi = Array.init rows (fun i -> i) in
  let a' = copy a in
    for j = 0 to cols - 1 do
      id_col.(j) <- 1.;
      if j > 0 then begin
	Array.blit a.data 0 a'.data 0 n;
	for k = 0 to rows - 1 do pi.(k) <- k; done;
      end;
      let lu, pi = lup_decomposition a' ~pi () in
	ignore (lup_solve lu ~x ~y pi id_col);
	for i = 0 to rows - 1 do
	  inv_dat.((base ~cols ~row:i) + j) <- x.(i);
	done;
	id_col.(j) <- 0.;
    done;
    inv


let inverse = lup_inverse


let scale t s =
  (** [scale t s] multiplies each element of [t] by the scalar [s].
      The result is the input matrix [t].

      This is an inplace version of the [scaled] function. *)
  let data = t.data in
  let n = Array.length data in
    for i = 0 to n - 1 do
      data.(i) <- data.(i) *. s
    done;
    t


let scaled t s =
  (** [scaled t s] returns a new matrix that is [t] with each element
      scaled by [s]. *)
  let ts = copy t in
    scale ts s


module Ops = struct
  (* Some functions that may be convenient but that may also pollute
     the namespace if included by default. *)

  (* Infix operations for matricies. *)
  let ( *// ) = product
  let ( +// ) = sum
  let ( -// ) = diff

  (* Simpler names for unary operations.  *)
  let det = determinant
  let inv = inverse
  let tr = transposed
  let scaled = scaled
end
