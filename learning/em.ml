(** expectation-maximization mixture of gaussians for d-dimensional
    data.

    @author eaburns
    @since 2009-08-21
*)

let normalize_components comps =
  let wt_sum = Array.fold_left (fun sum (_, wt) -> sum +. wt) 0. comps in
    Array.map (fun (g, wt) -> g, wt /. wt_sum) comps


let make_components ?(range=100.0) k d =
  (** [make_gaussians ?range k d] makes [k] random
      ([d]-dimensional gaussian * weight) tuples. *)
  Array.init k (fun _ ->
		  let g = Normal.hallucinate
		    ~mean_min:(~-.range) ~mean_max:range
		    ~std_max:range d
		  in g, (1. /. (float k)))


let estep components data =
  (** [estep components data] computes the probability that each datum
      belongs in each component. *)
  Array.map (fun datum ->
	       let datum_m = Matrix2d.of_vector datum in
	       let vec = Array.map (fun (g, wt) ->
				      let p = Normal.pdf g datum_m
				      in p *. wt)
		 components
	       in Vector.multiply_by vec (1. /. (Vector.sum vec)))
    data


let mean i pi data data_probs =
  (** [mean i pi data data_probs] computes a new mean for component
      [i].  [pi] is the probability of the given component, [data] is the
      data points and [data_probs] is the result of an estep. *)
  let pi = if pi = 0. then epsilon_float else pi in
  let n = Array.length data.(0) in
  let msum = Wrarray.fold_left2 (fun sum xj pj ->
				   let pij = pj.(i) in
				   let mu_t = Vector.multiply_by xj pij
				   in Vector.add mu_t sum)
    (Array.make n 0.) data data_probs
  in Vector.multiply_by msum (1. /. pi)


let cov i pi mi data data_probs =
  (** [cov i pi mi data data_probs] computes a new covariance matrix
      for component [i]. [pi] is the probability of the given
      component, [mi] is the mean of the component, [data] is the
      data points and [data_probs] is the result of an estep. *)
  let pi = if pi = 0. then epsilon_float else pi in
  let n = Array.length data.(0) in
  let csum = Wrarray.fold_left2 (fun sum xj pj ->
				   let pij = pj.(i) in
				   let m = [| Vector.subtract xj mi |] in
				   let cov_t = Matrix.mulf_scalar
				     (Matrix.mulf (Matrix.transpose m) m)
				     pij
				   in Matrix.add cov_t sum)
    (Array.make_matrix n n 0.) data data_probs
  in Matrix.mulf_scalar csum (1. /. pi)


let mstep components data data_probs =
  (** [mstep components data data_probs] compute a new set of components. *)
  let p = Array.mapi (fun i _ ->
			Array.fold_left (fun sum a -> sum +. a.(i))
			  0. data_probs)
    components
  in
  let comps = Array.mapi (fun i (g, wt) ->
			    let pi = p.(i) in
			    let m = mean i pi data data_probs in
			    let c = cov i pi m data data_probs
			    in Normal.of_arrays m c, pi)
    components
  in normalize_components comps


let fit_gaussians ?(iters=50) ?(range=100.0) k data =
  (** [fit_gaussians ?iters k data] fits [k] gaussians to [data] using
      [iters] iterations of EM. *)
  let d = Array.length data.(0) in
  let comps = ref (make_components ~range:range k d) in
    for i = 0 to iters - 1 do
      let e = estep !comps data in
	comps := mstep !comps data e
    done;
    Array.map fst !comps

(*
let plot_1d_norms norms xmin xmax step file =
  (** [plot_1d_components comps xmin xmax step file] plots a set of
      1-dimensional gaussians to the file [file].  [comps] is an array
      of gaussians (like the ones returned with [fit_gaussians]).  The
      functions are plotted starting at [xmin] and increasing by
      [step] until [xmax]. *)
  let line_1d t xmin xmax step i outch =
    (* make a line for a 1d gaussian. *)
    let x = ref xmin in
    let n = int_of_float ((xmax -. xmin) /. step) in
    let name = Printf.sprintf "line%d" i in
      Printf.fprintf outch "(%s (" name;
      for i = 0 to n - 1 do
	Printf.fprintf outch "(%f %f)" !x (Normal.pdf_1d t !x);
	x := !x +. step
      done;
      Printf.fprintf outch "))\n";
      name
  in
    Wrio.with_outfile file
      (fun outch ->
	 Printf.fprintf outch "(let* (\n";
	 let names =
	   Array.mapi (fun i g -> line_1d g xmin xmax step i outch) norms
	 in
	   Array.iter
	     (fun name ->
		Printf.fprintf outch
		  "(ds-%s (line-dataset :name \"%s\" :points %s))\n"
		  name name name)
	     names;
	   Printf.fprintf outch "(plot0 (num-by-num-plot\n";
	   Array.iter (Printf.fprintf outch ":dataset ds-%s\n") names;
	   Printf.fprintf outch "))\n)\n(display plot0))\n";
      )


(* The following lines will build an array of random numbers and will
   fit 3-gaussians to it then it will plot them. *)
let data = Array.init 100 (fun i ->
			     if i < 33
			     then [| (Random.float 10.) +. 10. |]
			     else
			       if i > 66
			       then [| (Random.float 10.) +. 50. |]
			       else [| (Random.float 20.) +. 80.|]);;

let gs = fit_gaussians 3 data;;

(*
plot_1d_norms gs 0. 120. 0.2 "output.spt";;
*)

*)
