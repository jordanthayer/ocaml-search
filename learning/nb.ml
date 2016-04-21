(** A Naive Bayes learner.  This learner doesn't assume that all data
    is equally likely.

    Based *heavily* off of Wheeler's implementation

    @author eaburns
    @since 2009-06-19
*)

open Printf

type t = {
  ccounts : int array;
  (* Number of times we have seen each class value. *)
  counts : int array array array;
  (* Number of times we have seen each feature attrubute per-class value. *)
  weight : int;
  (* Artificially weight the amount that each training instance counts
     as.  This can dampen artifacts of add-1 smoothing for places
     where we have only very few samples. *)
  mutable total : int;
  (* Total number of training data. *)
  nfeatures : int;
  (* Number of features in the vector. *)
  f_max : int;
  (* Max feature value. *)
  nclasses : int;
}

let make_add_1_smoothing ?(weight=10) nfeatures f_max nclasses =
  (** [make_add_1_smoothing nfeatures f_max nclasses] creates a new
      model with [nfeatures] features with a max value of [f_max] for
      each feature that classifies [nclasses].  This model uses
      add-one smoothing.

      @note The add-1 smoothing may be broken because of the datalikelyhood
      computation. *)
  {
    ccounts = Array.create nclasses nfeatures;
    counts = (Array.init
		nclasses
		(fun _ ->
		   Array.init nfeatures (fun _ -> Array.make (f_max + 1) 1)));
    weight = weight;
    total = (nclasses * (f_max + 1));
    nfeatures = nfeatures;
    f_max = f_max;
    nclasses = nclasses;
  }


let make_no_smoothing nfeatures f_max nclasses =
  (** [make_no_smoothing nfeatures f_max nclasses] creates a new model
      with [nfeatures] features with a max value of [f_max] for each
      feature that classifies [nclasses].  This model does not use any
      smoothing. *)
  {
    ccounts = Array.create nclasses 0;
    counts = (Array.init
		nclasses
		(fun _ ->
		   Array.init nfeatures (fun _ -> Array.make (f_max + 1) 0)));
    weight = 1;
    total = 0;
    nfeatures = nfeatures;
    f_max = f_max;
    nclasses = nclasses;
  }


let with_add_1_smoothing ?(weight=10) model =
  (** [with_add_1_smoothing ?width model] creates a new model, the
      same as the old one, but with +1 smoothing. *)
  { model with
      ccounts = (Array.map
		   (fun vl -> (vl * weight) + model.nfeatures)
		   model.ccounts);
      counts = (Array.map
		  (fun cary ->
		     Array.map (fun fary ->
				  Array.map
				    (fun vl -> (vl * weight) + 1)
				    fary)
		       cary)
		  model.counts);
      weight = model.weight * weight;
      total = model.total + (model.nfeatures * (model.f_max + 1));
  }


let make = make_add_1_smoothing
  (** [make nfeatures f_max nclasses] creates a new model with
      [nfeatures] features with a max value of [f_max] for each
      feature that classifies [nclasses].  This model uses the default
      smoothing method.  *)


let train model instance =
  (** [train model instance] trains on [instance], a (label * feature)
      tuple. *)
  let w = model.weight in
  let cls, features = instance in
    model.ccounts.(cls) <- model.ccounts.(cls) + w;
    model.total <- model.total + w;
    for i = 0 to model.nfeatures - 1 do
      let fi = features.(i) in
	model.counts.(cls).(i).(fi) <- model.counts.(cls).(i).(fi) + w
    done


let save model ch =
  (** [save model ch] saves [model] to the output channel [ch]. *)
  Marshal.to_channel ch model []


let load ch =
  (** [load ch] loads the model from the input channel [ch]. *)
  (Marshal.from_channel ch : t)


let raw_prob model features cls =
  (** [raw_prob model features cls] computes the non-normalized
      probability of the class label [cls] given the [features]. *)
  let ncls = float model.ccounts.(cls) in
  let prob = ref (ncls /. (float model.total)) in
    for i = 0 to model.nfeatures - 1 do
      let fi = features.(i) in
	if model.counts.(cls).(i).(fi) = 0
	then prob := 0.0
	else
	  if ncls <> 0.0
	  then prob := !prob *. (float model.counts.(cls).(i).(fi) /. ncls)
    done;
    !prob


let prob model features cls =
  (** [prob model features cls] computes the probability of the class
      label [cls] given the [features]. *)
  let sum = ref 0.
  and vl = ref 0. in
    for c = 0 to model.nclasses - 1 do
      let p = raw_prob model features c in
	if c = cls then vl := p;
	sum := !sum +. p;
    done;
    if !sum = 0. || !vl = 0.
    then 0.
    else
      let p = !vl /. !sum in
	if p > 1.0
	then failwith (sprintf "p=%f > 1.0, !vl=%f, !sum=%f" p !vl !sum)
	else p


