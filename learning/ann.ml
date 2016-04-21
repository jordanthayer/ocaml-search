(**
    @author jtd7
    @since 2010-10-13

   Artificial Neural networks
*)

type neuron = {
  input_weights : float array;
  activation_function : (float -> float);
  activation_deriv : (float -> float);
}


type one_layer = neuron

type two_layer = {
  hidden_layer : neuron array;
  output_neuron : neuron;
}

type stored_neuron = {
  weights : float array;
  activation : string;
}

open Float_ref

(****** Some sample activation functions **************)
let make_threshold t v =
  if v >= t then 1. else 0.

let sigmoid v =
  1. /. (1. +. (exp ~-.v))

let d_sigmoid x =
  (sigmoid x) *. (1. -. (sigmoid x))


let output neuron (features : float array) =
  (* computes the output of single neuron *)
  assert ((Array.length features) = (Array.length neuron.input_weights));
  let input = float_ref 0. in
    for i = 0 to ((Array.length features) - 1) do
      input <-- !!input +. neuron.input_weights.(i) *. features.(i)
    done;
    neuron.activation_function !!input


(*************************** Two layer functions ******************************)

let one_layer_output network (features : float array) =
  (** Does output on a single layer (perceptron) network *)
  output network features

let show_example_one_layer ?(alpha = 0.01) network features target =
  let estimate = one_layer_output network features in
  let error = target -. estimate in
    for i = 0 to ((Array.length network.input_weights) - 1) do
      network.input_weights.(i) <-
	network.input_weights.(i) +.
	alpha *. error *. (network.activation_deriv features.(i)) *.
	features.(i)
    done;
    error

(*************************** Three layer functions ****************************)
let two_layer_output network (features : float array) =
  (** does output on a two layer network *)
  output network.output_neuron
    (Array.map (fun hneuron -> output hneuron features) network.hidden_layer)


let show_example_twolayer ?(alpha = 0.01) network features target =
  (* Back prop *)
  (* going forward to get estimates at each layer *)
  let hidden_values = (Array.map (fun n -> output n features)
			 network.hidden_layer) in
  let estimate = output network.output_neuron hidden_values in
  let delta_i = (target -. estimate) *.  alpha
  and it_leng = Array.length network.hidden_layer - 1
  and old_iweight = Array.copy network.output_neuron.input_weights in
      (* output layer *)

      for i = 0 to it_leng do
	network.output_neuron.input_weights.(i) <-
	  network.output_neuron.input_weights.(i) +.
	  hidden_values.(i) *. delta_i
      done;

      (* hidden layer *)
      for i = 0 to it_leng do

	let isum = (Wrarray.fold_left2 (fun accum wt v -> accum +. wt *. v)
		      0. features network.hidden_layer.(i).input_weights) in

	let delta_j = ((network.hidden_layer.(i).activation_deriv isum) *.
			 old_iweight.(i) *. delta_i)in

	  for j = 0 to (Array.length features) - 1 do
	    network.hidden_layer.(i).input_weights.(j) <-
	      network.hidden_layer.(i).input_weights.(j) +.
	      features.(j) *. delta_j
	  done
      done;
      abs_float delta_i


let batch_train_twolayer ?(ethresh = 10.) ?(nepoch = 500)
    network features targets =
  let max_error = float_ref infinity in
  let samples = Array.length features
  and counter = ref 0 in
    while !!max_error > ethresh && !counter < nepoch do
      (max_error <-- 0.;
      (for i = 0 to samples - 1 do
	 max_error <-- max !!max_error (show_example_twolayer network
					features.(i) targets.(i))
       done);
      counter := !counter + 1;
      (let pd = (!counter * 100) / nepoch in
	 Verb.pe Verb.toplvl "Max error %f \t%i%% done\n%!" !!max_error pd))
    done


(************************ Constructing Networks *******************************)
let perceptron feature_length threshold =
  { input_weights = Array.init feature_length (fun i -> Random.float 1.);
    activation_function = make_threshold threshold;
    activation_deriv = (fun _ -> 1.); }


let two_layer feature_length hidden_units =
  { hidden_layer = (Array.init hidden_units
		      (fun i ->
			 { input_weights = (Array.init feature_length
					      (fun i -> Random.float 1.));
			   activation_function = sigmoid;
			   activation_deriv = d_sigmoid;}));

    output_neuron = {input_weights = (Array.init hidden_units
					(fun i -> Random.float 1.));
		     activation_function = Fn.identity;
		     activation_deriv = (fun i -> 1.)};}



(************************** I/O **************************************)
let print_weights_neuron n =
  Array.iter (Verb.pe Verb.always "%f\t") n.input_weights;
  Verb.pe Verb.always "\n%!"

let print_weights network =
  Array.iter print_weights_neuron network.hidden_layer;
  print_weights_neuron network.output_neuron


let stored_to_neuron sn =
  match sn.activation with
    | "identity" -> { input_weights = sn.weights;
		      activation_function = Fn.identity;
		      activation_deriv = (fun _ -> 1.); }
    | "sigmoid" -> { input_weights = sn.weights;
		     activation_function = sigmoid;
		     activation_deriv = d_sigmoid; }
    | _ -> failwith "Unrecognized activation function"


let save_two_layer network file_name =
  (** Saves a two layer neural net to a file.  Assumes the standard
      configuration of activation functions since I can't marshal those in
      any reasonable manner *)
  let ch = open_out file_name in
  let hl = (List.map (fun neuron ->
			{ weights = neuron.input_weights;
			  activation = "sigmoid" })
	      (Array.to_list network.hidden_layer))
  and on = { weights = network.output_neuron.input_weights;
	     activation = "identity";} in
    Marshal.to_channel ch (on::hl) [];
    close_out ch

let save_preceptron network file_name =
  (** Saves a perceptron to a file.  Assumes the standard
      configuration of activation functions since I can't marshal
      those in any reasonable manner *)
    let ch = open_out file_name in
      Marshal.to_channel ch { weights = network.input_weights;
			      activation = "identity";} [];
      close_out ch


type network =
    (* I needed this to get the loading code down to one function *)
  | Perceptron of one_layer
  | Twolayer of two_layer


let load_network file_name =
  let ch = open_in file_name in
  let snw = Marshal.from_channel ch in
    close_in ch;
    match snw with
      | [] -> failwith "Nothing to load"
      |	[sp] -> Perceptron (stored_to_neuron sp)
      | output::hidden ->
	  Twolayer
	    { hidden_layer = Array.of_list (List.map stored_to_neuron hidden);
	      output_neuron = stored_to_neuron output; }



let ann_to_string network =
  (** Saves a two layer neural net to a file.  Assumes the standard
      configuration of activation functions since I can't marshal those in
      any reasonable manner *)
  let hl = (List.map (fun neuron ->
			{ weights = neuron.input_weights;
			  activation = "sigmoid" })
	      (Array.to_list network.hidden_layer))
  and on = { weights = network.output_neuron.input_weights;
	     activation = "identity";} in
    Marshal.to_string (on::hl) []

let ann_from_string str =
  let snw = Marshal.from_string str 0 in
    match snw with
      | output::hidden ->
	  Twolayer
	    { hidden_layer = Array.of_list (List.map stored_to_neuron hidden);
	      output_neuron = stored_to_neuron output; }
      | _ -> failwith "not a two layer neural network"

(************************ Testing *******************************)
let test_one_layer ?(mag = 100.) ?(samples = 1000) target_function features =
  let network =
    {input_weights = (Array.init (Array.length (features 0.))
			(fun i -> Random.float 1.));
     activation_function = Fn.identity;
     activation_deriv = (fun _ -> 1.)} in

    for i = 0 to (samples - 1) do
      (let x = Random.float 100. in
	 ignore (show_example_one_layer network (features x)
		   (target_function x)))
    done;
    Verb.pe Verb.always "Source\t\tTruth\t\tLearned\t\tError\n";
    for i = 0 to 10 do
      (let x = Random.float mag in
       let target = target_function x
       and est = one_layer_output network (features x) in
       let error = target -. est in
	 Verb.pe Verb.always "%f\t%f\t%f\t%f\n%!" x target est error)
    done


let test_two_layer ?(mag = 100.) ?(samples = 10000) target_function features =
  let network = two_layer (Array.length (features 0.)) 3 in

    for i = 0 to (samples - 1) do
      (let x = Random.float 10. in
	 ignore (show_example_twolayer network (features x)
		   (target_function x)))
    done;
    Verb.pe Verb.always "Source\t\tTruth\t\tLearned\t\tError\n";
    for i = 0 to 20 do
      (let x = Random.float mag in
       let target = target_function x
       and est = two_layer_output network (features x) in
       let error = target -. est in
	 Verb.pe Verb.always "%f\t%f\t%f\t%f\n%!" x target est error)
    done


let batch_test_two_layer ?(mag = 100.) ?(samples = 10000)
    target_function features_f =
  let network = two_layer (Array.length (features_f 0.)) 3 in
  let features = Array.init samples (fun i -> [||])
  and targets = Array.init samples (fun i -> nan) in
    for i = 0 to (samples - 1) do
      let v = Random.float mag in
	features.(i) <- features_f v;
	targets.(i) <- target_function v
    done;
    batch_train_twolayer network features targets;
      Verb.pe Verb.always "Source\t\tTruth\t\tLearned\t\tError\n";
      for i = 0 to 20 do
	(let x = Random.float mag in
	 let target = target_function x
	 and est = two_layer_output network (features_f x) in
	 let error = target -. est in
	   Verb.pe Verb.always "%f\t%f\t%f\t%f\n%!" x target est error)
      done


(* EOF *)
