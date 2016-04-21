(**

   Does analysis on datasets that are grouped.

*)


let fs_compare s1 s2 = 
  (**float/string compare.  

     First tries to compare the two items as floats, then if that
     doesn't work just compares them as strings.  The things to be
     compared are both strings, but if the string can be viewed as a
     float, it is treated as a float.*)
  try 
    (let f1 = float_of_string s1 
     and f2 = float_of_string s2 
     in compare f1 f2)
  with Failure "float_of_string" -> (compare s1 s2)


let print_tuple t = 
  let (gp, gv) = t in
    Printf.sprintf "%s %s" gp gv


let print_header hl = Wrlist.sprint print_tuple " " hl


type aggregator = 
    Count
  | Mean
  | Median
  | Max
  | Min
  | Stdev


let get_grouped_report ?(report_value="") ds gc report_type  =
  (**
     makes a grouped report.  Groups by the items in [gc] and gives
     [report_type] of report.  Count gives a count, mean and median
     calculate that number for the field specified by [report_value]

     ds is the dataset that the report shuld be run on.
  *)
  let grouped_data =  Dataset.group_by 
    ~compare:fs_compare
    ~sort:Dataset.Ascending
    (Array.of_list gc) ds in
    List.iter (
      fun this_ds -> 
	let header_data = List.combine 
	  gc
	  (List.map 
	     (fun v -> Dataset.get_group_value v this_ds) gc) in
	let header_string = print_header header_data in
	let output_string = 
	  match report_type with
	      Count -> (float_of_int (Dataset.count this_ds))
	    | Mean -> (Dataset.get_mean report_value this_ds)
	    | Median -> (Dataset.get_median report_value this_ds) 
	    | Max -> (Dataset.max_of_datasets report_value [this_ds])
	    | Min -> (Dataset.min_of_datasets report_value [this_ds])
	    | Stdev -> (Dataset.get_stdev report_value this_ds)
	in
	  Printf.printf "%-60s %10.3f\n" header_string output_string
    ) grouped_data
