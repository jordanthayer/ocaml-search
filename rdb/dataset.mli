(** A dataset allows for manipulating whole sets of datafiles at the
    same time.

    @author eaburns
    @since 02-11-2009
*)

type t

type sorting =
  | Unsorted
  | Ascending
  | Descending
  | First
  | Last

val get_dfs : t-> Datafile.t list

val get_name : t -> string

val with_name : string -> t -> t

val identity : 'a -> 'b -> 'b

val merge : (t list) -> t

val empty : string -> t
  (** [empty name] makes an empty dataset with the given name. *)

val make_dummy_on_fail : string -> Datafile.t

val load_from_rdb :
  ?on_fail:(string -> Datafile.t) ->
  ?skip_fails:bool -> db:string ->
  Rdb.attrs -> name:string -> t
  (** [load_from_rdb db attrs] loads a dataset from the RDB database
      [root] given the list of attribute tuples. *)


val load_from_rdb_with_domain :
  ?skip_fails:bool ->
  domain:string -> Rdb.attrs -> name:string -> t
  (** [load_from_rdb_with_domain domain attrs] loads a dataset from
      the RDB database for [domain] by using the user's data_root
      directory from experiments. *)

val concat_data : t -> Datafile.t list -> t
  (** [concat_data t dfs] adds the datafiles [dfs] to the dataset
      [t]. *)

val transform : string -> (Datafile.t -> float -> float) -> t -> t
  (** [transform key tr t] a new dataset with the transformation
      function [tr] applied to the values for the [key].  [tr] accepts
      a datafile and the value from that datafile for [key].  [tr]
      results in a float. *)

val numeric_transform : string -> (float -> float) -> t -> t
  (** [numeric_transform key tr t] a new dataset with the transformation
      function [tr] applied to the values for the [key].  [tr] accepts
      a float and results in a float. *)

val transform_with :
  string array ->
  t ->
  string ->
  ?with_key:string -> (float -> float -> float) -> t -> t
  (** [transform_with match_keys with_ds key ?with_key tr t] a new
      dataset that transforms values of [key] by finding a datafile in
      [by_dfs] with the same [match_keys] as the current datafile and
      calling [tr] with both values.  For example you can use this to
      normalize to A* or to get a speedup dataset using
      [match_key]="num" and [with_ds] being the dataset for A*.

      [tr] is a function that takes the value first from the 'with
      dataset' and then from the dataset being transformed and results
      in a new value for the given key.

      [with_key] lets you specify an optional key to use to load the
      value from the [with_ds].  This is useful when the value in the
      A* set that you want to normalize on is not the same key as the
      value in the set being normalized.

      NOTE: The [match_keys] do string matching and none of the
      transforms performed on these fields are taken into account.

      NOTE: any transformations that you might have made on the
      [with_ds] dataset are not yet taken into account.  This will be
      fixed in a future version.  *)

val copy_key : key:string -> new_key:string -> t -> t
  (** [copy_key key new_key t] makes a (logical) copy of the data
      accessed by [key] under the new name [new_key].  This copies all of
      the transforms for [key] too. *)


val new_key : string -> (Datafile.t -> float) -> t -> t
  (** [new_key new_key f t] creates a new key that is a function of
      each datafile. *)


val filter : (string -> 'a) -> ('a -> bool) -> string -> t -> t
  (** [filter kind f key ds] filters a dataset.  [f] is evaluated with
      the value for [key] on each row.  Rows for which [f] evaluated
      to false are removed in the result. *)


val group_by :
  ?compare:(string -> string -> int) ->
  ?sort:sorting ->
  ?name_fun:(string -> string -> string -> string) ->
  string array -> t -> t list
  (** [group_by ?sort ?name_fun keys t] gets a list of datasets
      where each dataset has the same value for each key in [keys].
      Note: no key can be a column key.  [name_fun] is called with
      the original name, the key name and the value of the grouping
      key to get the name of each group. *)

val get_group_value : string -> t -> string
  (** [get_group_value key t] gets the value of a 'group key'.  That
      is, the value of a key that was once used in a group_by
      operation to group this dataset.  All of the datafiles in this
      set have the same value for this key.

      {b NOTE:} If the value of the key is a number then it will end
      with a '.' character reguardless of whether or not it actually
      had one in the datafile or not. *)

val get_values : (string -> 'a) -> ?nullfill:bool ->
  ?sort:sorting -> string -> t -> 'a array
  (** [get_values kind ?sort key t] gets all of the values for the
      given key.  If the key is a column key in a datafile, the values
      are all append into one big array. [kind] is a conversion
      function from a string to the desired final type

      {b NOTE:} If the value of the key is a number then it will end
      with a '.' character reguardless of whether or not it actually
      had one in the datafile or not. *)

val get_row_vector :
  (string -> 'a) -> ?nullfill:bool -> ?sort:sorting ->
  string array -> t -> 'a array array
  (** [get_row_vector kind ?sort keys t] gets a vector of rows of data
      where each column of a row is the value for the respective key
      in the [keys] array.  [kind] is a conversion function from a
      string to the desired type. *)

val get_column_vector :
  (string -> 'a) -> ?sort:sorting -> string array -> t -> 'a array array
  (** [get_column_vector kind ?sort keys t] gets a vector of columns
      of data where each column is for the respective key in the
      [keys] array. [kind] is a conversion function from a string to
      the desired type. *)

val new_keys : t -> string list
  (** [new_keys t] gets a list of the names of all newly defined keys.
      That is, all of the keys that are not specifically defined in the
      datafiles. *)

val is_key : string -> t -> bool

val keys : t -> string list
  (** [keys t] gets a list of the names of all keys.  This assumes
      that all datafiles in the set have the same set of keys. *)

val raw_dfs : t -> Datafile.t list
  (** [raw_dfs t] gets a list of the raw datafiles. *)

val name : t -> string
  (** [name t] gets the name of the dataset. *)

val fold : string -> ('a -> float -> 'a) -> 'a -> t -> 'a
  (** [fold key f init t] folds a function across all of the values
      for a given key. *)

val get_min : string -> t -> float
  (** [get_min key t] gets the minimum value for a given key in
      the dataset. *)

val get_max : string -> t -> float
  (** [get_max key t] gets the maximum value for a given key in
      the dataset. *)

val get_mean : string -> t -> float
  (** [get_mean key t] gets the mean value for a given key in
      the dataset. *)


val get_finite_mean : string -> t -> float
  (** [get_mean key t] gets the mean value for a given key in
      the dataset. *)


val get_median : string -> t -> float
  (** [get_median key t] gets the median value for a given key in the
      dataset. *)

val get_variance : string -> t -> float
  (** [get_variance key t] gets the sample variance for values for a
      given key in the dataset. *)

val get_stdev : string -> t -> float
  (** [get_stdev key t] gets the sample standard deviation for values
      for a given key in the dataset. *)

val get_stderr : string -> t -> float
  (** [get_stderr key t] gets the standand error on the mean for values
      for a given key in the dataset.  This is how you compute a confidence
      interval. *)

val min_of_datasets : string -> t list -> float
  (** [min_of_datasets key ts] gets the minimum value for [key] from a
      list of datasets. *)

val max_of_datasets : string -> t list -> float
  (** [max_of_datasets key ts] gets the maximum value for [key] from a
      list of datasets. *)

val count : t -> int
  (** [count ts] gets the number of items in the dataset. *)


val to_line :
  xkey:string -> ykey:string -> t -> (float * float) array * string
  (** [to_line xkey ykey t] gets data suitable for line-plotting in
      ps-plot. *)

val to_scatter :
  ?glyph:string ->
  xkey:string ->
  ykey:string -> t -> string * string * (float * float) array
  (** [to_scatter ?glyph xkey ykey t] gets data suitable for
      scatter-plotting in ps-plot. *)

val to_line_and_errbars :
  ?sort:sorting ->
  ?final_xkey:string ->
  string array ->
  xkey:string ->
  ykey:string -> t -> (float array * float array) list * string
  (** [to_line_and_errbars ?final_xkey group_by xkey ykey t] get data
      suitable for line_and_errbars plotting in ps-plot.  The data is
      grouped on [group_key].

      If [final_xkey] is specified then an extra data point is added
      at the x value given by the [final_xkey] for each data point
      which has the y-value equal to the last y value.  This is useful
      for anytime profiles. *)

val to_csv_string : ?sort:sorting -> ?header:bool -> string array -> t -> string
  (** [to_csv_string keys t] gets a comma seperated value string for
      the given keys in the dataset.  This can then be printed to a
      file. *)


val size : t -> int

val add_value : string -> (Datafile.t -> string) -> t -> unit

val add_final_value : string -> string -> t -> unit

val histogram : string -> t -> ((string * int) list)

val ds_exists : string -> string -> t -> bool
