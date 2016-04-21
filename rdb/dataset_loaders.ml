let load_algs ?(skip_fails=false) alg_list attrs domain = 
  let alg_ds = List.map (
    fun alg -> 
      Dataset.load_from_rdb_with_domain 
	~skip_fails:true
	~domain:domain
	(("alg",alg)::attrs) 
	~name:(alg^"_"^domain^"_data")
  ) alg_list in
    Dataset.merge alg_ds


let add_param ds = 
  Dataset.add_value "parameter" 
    (
      fun df ->
	if Datafile.has_val df "beam_width" then
	  Datafile.get_val df "beam_width"
	else if Datafile.has_val df "wt" then
	  Datafile.get_val df "wt"
	else
	  "0."
    ) ds
