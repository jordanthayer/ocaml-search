(** Operations on directed graphs *)

(** Constructs a directed planar graph by placing random points on the
    unit squrae and performing a Delaunay triangulization.  Each edge
    is directed toward the positive x direction.  The graph is not
    guaranteed to be connected.  The result is an edge list where each
    node is represented by an integer.  Additionally an array mapping
    nodes to their respective point is returned. *)
let random_planar_triangles nnodes =
  let nodes = Array.init nnodes (fun _ -> Random.float 1., Random.float 1.) in
  let triangulation = Delaunay.Float.triangulate nodes in
  let nodes = Hashtbl.create 257 in
  let points = Array.create nnodes (nan, nan) in
  let next_node = ref 0 in
  let get_node p =
    try Hashtbl.find nodes p with Not_found ->
      let n = !next_node in
        Hashtbl.add nodes p n;
        points.(n) <- p;
        incr next_node;
        n in
  let add_edge p0 p1 lst =
    let x0, y0 = p0 and x1, y1 = p1 in
    let dist = sqrt ((x1 -. x0) ** 2. +. (y1 -. y0) ** 2.) in
    let n0 = get_node p0 and n1 = get_node p1 in
      if x0 < x1 then
        (n0, n1, dist) :: lst
      else
        (n1, n0, dist) :: lst
  in
    Delaunay.Float.fold add_edge triangulation [], points

(** Dumps a spt-it-out input file to the file [fname] to draw the
    graph. *)
let plot edges points fname =
  let fmt f = Format.fprintf Format.str_formatter f in
    fmt "(let* (@[";
    Wrlist.iteri
      (fun i (n0, n1, _) ->
         let x0, y0 = points.(n0) and x1, y1 = points.(n1) in
           fmt "(@[edge%d@ ((%g %g) (%g %g))@])@\n" i x0 y0 x1 y1;
           fmt "(@[scatter%d@ (line-points-dataset " i;
           fmt ":glyph \"circle\" :dashes () ";
           fmt ":points edge%d)@])@\n" i)
      edges;
    fmt "(@[plot@ (num-by-num-plot @[";
    Wrlist.iteri (fun i _ -> fmt ":dataset scatter%d@\n" i) edges;
    fmt "@])@])@\n";
    fmt "@])@\n(@[(display plot)@]))";
    Wrio.with_outfile fname
      (fun outch -> output_string outch (Format.flush_str_formatter ()))

let left_most points =
  let min_x = ref (fst points.(0)) and min_i = ref 0 in
    for i = 1 to Array.length points - 1 do
      let x, _ = points.(i) in
        if x < !min_x then begin
          min_x := x;
          min_i := i;
        end
    done;
    !min_i

(** Finds the clockwise angle between line segments p0->p1 and
    p1->p2. *)
let cw_angle p0 p1 p2 =
  let ( * ) (x0, y0) (x1, y1) = x0 *. x1 +. y0 *. y1 in
  let ( - ) (x0, y0) (x1, y1) = x1 -. x0, y1 -. y0 in
  let ( / ) (x, y) s = x /. s, y /. s in
  let len p = sqrt (p * p) in
  let a = p1 - p0 and b = p2 - p1 in
  let a_norm = a / (len a) and b_norm = b / (len b) in
    Math.pi +. (acos (a_norm * b_norm))

(** Gets all of the edges from the given point. *)
let point_edges edges pt =
  List.filter (fun (p, p', _) -> p = pt || p' = pt)
    (edges : (int * int * float) list)

(** Get the edge with the minimum clockwise angle from the
    line-segment p_prev->p_cur. *)
let min_angle_edge points p_prev p_cur n_cur edges =
  fst (Wrlist.fmin_by
         (fun (src, dst, _) ->
            let n_next = if src = n_cur then dst else src in
            let p_next = points.(n_next) in
            let theta = cw_angle p_prev p_cur p_next in
              if p_next = p_prev then infinity else theta)
         edges)

(** Get the list of edges along the outter hull of the given graph.
    This uses the 'gift wrapping algorith.' *)
let outter_hull edges points =
  let l = left_most points in
  let rec add_edges accum p_prev n_cur =
    let edges = point_edges edges n_cur in
    let p_cur = points.(n_cur) in
    let next = min_angle_edge points p_prev p_cur n_cur edges in
    let cur_node, next_node =
      let src, dst, _ = next in if n_cur = src then src, dst else dst, src
    in
      assert (cur_node = n_cur);
      if next_node  <> l then
        add_edges (next :: accum) p_cur next_node
      else
        next :: accum
  in
  let x0, y1 = points.(l) in
    add_edges [] (x0, y1 -. 1.) l
