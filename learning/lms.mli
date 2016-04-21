(* $Id: lms.ml,v 1.1 2003/07/18 21:22:40 ruml Exp ruml $

   on-line linear regression


   given number of paramaters, all functions return 3 functions:

   show_example (given pattern and target value, returns estimate and
   error)

   compute_estimate (given pattern, return prediction), and

   get_weights (returns the current linear hypothesis).
*)


val init_lms :
  ?initial_weights: float array option ->
  ?initial_weight:float ->
  ?learning_rate:float ->
  ?bias:(float -> float) ->
  int ->
  (float array -> float -> float * float) * (float array -> float) *
  (unit -> float array)

val init_lms2 :
  ?initial_weights: float array option ->
  ?initial_weight:float ->
  ?learning_rate:float ->
  int ->
  (float array -> float -> float * float) * (float array -> float) *
  (unit -> float array)


val init_nlms_momentum :
  ?initial_weights: float array option ->
  ?initial_weight:float ->
  ?learning_rate:float ->
  ?momentum_rate:float ->
  ?exp_sq_noise:float ->
  int ->
  (float array -> float -> float * float) * (float array -> float) *
  (unit -> float array)


val init_nlms_flow :
  ?initial_weights: float array option ->
  ?initial_weight:float ->
  ?initial_learning_rate:float ->
  ?meta_learning_rate:float ->
  ?exp_sq_noise:float ->
  ?normalization_scale:float ->
  ?leakiness:float ->
  ?max_learning_rate:float ->
  ?min_learning_rate:float ->
  int ->
  (float array -> float -> float * float) * (float array -> float) *
  (unit -> float array)


val init_nlms_flow_default :
  int ->
  (float array -> float -> float * float) * (float array -> float) *
  (unit -> float array)


(**** debugging *****)


val test :
  ?trials:int ->
  ?verb:bool ->
  ?seed:int ->
  (int ->
     (float array -> float -> float * float)
     * (float array -> float) *
     (unit -> float array)) ->
  unit

val test2 :
  ?trials:int ->
  ?verb:bool ->
  ?seed:int ->
  (int ->
     (float array -> float -> float * float)
     * (float array -> float) *
     (unit -> float array)) ->
  (float -> float) ->
  (float -> float array) ->
  unit


(* EOF *)
