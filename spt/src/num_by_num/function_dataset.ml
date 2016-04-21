(** Plotting of an arbitrary function.

    @author jtd7, eaburns
    @since 2010-05-10
*)

open Num_by_num_dataset
open Drawing
open Geometry
open Verbosity


(** A line plot dataset. *)
class function_dataset
  dashes ?(samples=200) ?(line_width=Length.Pt 1.) ?(color=black) ?name f =
object (self)
  inherit Num_by_num_dataset.dataset ?name ()

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = line_width;
    }


  (** [points src] computes the points *)
  method private points src =
    let dx = (src.x_max -. src.x_min) /. (float (samples - 1)) in
    let x = ref src.x_min in
    let pts = ref [] in
      while Geometry.sloppy_float_leq !x src.x_max do
	let y = f !x in
	  pts := (point ~x:!x ~y) :: !pts;
	  x := !x +. dx;
      done;
      List.rev !pts


  (** [dimensions] gets the dimensions.  Return an 'inverse rectangle'
      so that this *should* have no effect on the dimensions of the
      plot. *)
  method dimensions =
    rectangle ~x_min:infinity ~x_max:neg_infinity
      ~y_min:infinity ~y_max:neg_infinity


  method mean_y_value src =
    let points = self#points src in
    let s, n =
      List.fold_left (fun (s, n) p -> s +. p.y, n + 1) (0., 0) points
    in s /. (float n), n


  method draw ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      draw_line ctx ~box:src ~tr ~style (self#points src)


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units Line_dataset.line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]

  method legend_dimensions ctx =
    (ctx.units Line_dataset.line_legend_length), (ctx.units line_width)

  method avg_slope = nan

end


let function_dataset dashes ?samples ?line_width ?color ?name f =
  (** [function_dataset dashes ?samples ?line_width ?color ?name f]
      makes a new function dataset. *)
  new function_dataset dashes ?samples ?line_width ?color ?name f


(** {2 Scatter plot with best fit} ****************************************)

open Lacaml.Impl.D

let poly_features degree x =
  (** [poly_features degree x] get a polynomial feature array. *)
  Array.init (degree + 1) (fun d -> x ** (float d))


let compute_poly degree coeffs x =
  (** [compute_poly degree coeffs x] compute a polynomial in [x] given
      the coefficients in a bigarray. *)
  let vl = ref 0. and t = ref 1. in
    for i = 0 to degree do
      let c = coeffs.{i + 1, 1} in
	vl := !vl +. c *. !t;
	t := !t *. x;
    done;
    !vl


let term_string coeffs i =
  (** [term_string coeffs i] gets the string representation of a
      single term. *)
  let coeff = coeffs.{i + 1, 1} in
  let cstr = Printf.sprintf "%g" coeff in
    if i = 0
    then Some cstr
    else begin
      let cstr = if cstr = "1" then "" else cstr in
	if i = 1
	then Some (cstr ^ "x")
	else Some (Printf.sprintf "%sx^%d" cstr i)
    end


let poly_string degree coeffs =
  (** [poly_string degree coeffs] get a string of the polynomial given
      the coefficients. *)
  let terms = ref "" in
    for i = 0 to degree do
      match term_string coeffs i with
	| Some term ->
	    if !terms = ""
	    then terms := term
	    else terms := Printf.sprintf "%s + %s" term !terms
	| None -> ()
    done;
    "y = " ^ !terms


let bestfit_dataset
    ~glyph ~dashes ?color ?line_width ?point_radius ?(degree=1)
    ?(fit_in_name=true) ?name points =
  let scatter =
    new Scatter_dataset.scatter_dataset
      glyph ?color ?point_radius ?name points in
  let xs =
    Mat.of_array (Array.map (fun p -> (poly_features degree p.x)) points) in
  let ys = Mat.of_array (Array.map (fun p -> [| p.y |]) points) in
    ignore (gelsd ~rcond:1e-20 xs ys);
    let samples = if degree = 1 then Some 2 else None in
    let poly =
      new function_dataset dashes ?samples ?line_width ?color ?name
	(compute_poly degree ys)
    in
    let name =
      if fit_in_name
      then
	match name with
	  | None -> Some (Printf.sprintf "(%s)" (poly_string degree ys))
	  | Some n -> Some (Printf.sprintf "%s (%s)" n (poly_string degree ys))
      else name
    in
      new composite_dataset ?name [scatter; poly;]


let bestfit_datasets ?(color=false)
    ?point_radius ?line_width ?degree ?fit_in_name
    name_by_point_list_list =
  let next_glyph = Factories.default_glyph_factory () in
  let next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, point_list) ->
		bestfit_dataset
		  ~glyph:(next_glyph ())
		  ~dashes:(next_dash ())
		  ~color:(next_color())
		  ?line_width ?point_radius ?degree
		  ?fit_in_name
		  ?name point_list)
      name_by_point_list_list

(* EOF *)
