(** Cross-validation for learning algorithms.

    @author eaburns
    @since 2009-09-02
*)

open Float_ref

module Leave_one_out = struct

  let leave_out_nth training n =
    (** [leave_out_nth training n] gets a tuple of the training data
	without the [n]th value and the [n]th value. *)
    training.(n),
    Array.init
      ((Array.length training) - 1)
      (fun i -> if i < n then training.(i) else training.(i + 1))


  let max_numeric ?(tolerance=0.01) ?(max_evals=1000)
      leaveouts training score make_learner a b =
    (** [max_numeric ?tolerance ?max_evals leaveouts training score
	make_learner a b] performs leave-one-out cross validation for a
	learner that takes a single floating point parameter.  This
	function attempts to find the best value for the parameter in
	the range [a]-[b] in order to maximize the score function for
	the estimated values of the learner on the training data.

	[leaveouts] is an array of the indices to leave out.

	[training] is the training data set array.

	[score] returns a numeric score given the actual value and the
	estimated value for the learner.  This score is going to be
	maximized.

	[make_learner] is a function that takes the training data and a
	single floating point parameter (which we are trying to find the
	best value for) and returns a function that takes data points
	and returns floating point scores. The type of [make_learner]
	is: [('a * 'b) array -> float -> ('a -> 'b)]

	[a]-[b] define the range of values to try for the parameter. *)

    let rounds = Array.map (leave_out_nth training) leaveouts in

    let neg_sum_score param =
      (* this is the function that we try to minimize... the negative
	 sum of the scores. *)
      let n = Array.length rounds in
      let sum = float_ref 0. in
	for i = 0 to n - 1 do
	  let (x, y), training' = rounds.(i) in
	  let est = make_learner training' param in
	    sum <-- !!sum +. (score y (est x))
	done;
	~-. !!sum
    in
      fst (Num_opt.nearby_min neg_sum_score a b tolerance max_evals)


  let minimize_sse ?tolerance ?max_evals ?(max_leaveouts=500)
      training make_learner a b =
    (** [minimize_sse ?tolerance ?max_evals ?max_leaveouts training
	make_learner a b] tries to find the setting for a numeric
	parameter that minimizes the sum of the square error for the
	given function approximation on the training data. *)
    let score actual estimate = ~-. ((actual -. estimate) ** 2.) in
    let n = Array.length training in
    let leaveouts =
      if n > max_leaveouts then
	Array.init n (fun _ -> Random.int n)
      else
	Array.init n Fn.identity
    in
      max_numeric ?tolerance ?max_evals leaveouts training score
	make_learner a b

end
