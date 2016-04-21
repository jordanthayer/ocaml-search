(* $Id: datafile.mli,v 1.2 2008/01/11 01:01:21 ruml Exp ruml $

   easily parsing files of experimental results.

   datafiles hold two things: key-value pairs (both are strings,
   things like "alg"="a-star") and columns (arrays of floats).
   Columns come in two types: regular and "alternative".  The
   intention is that most rows in the data file will be regular
   colunms (eg, "best sol cost" "raw cpu time") - just floats
   separated by whitespace (presumably tabs).  This data is put into
   columns according the "colnames" that are defined in the file.

   But sometimes one wants to log other data too.  So in addition to
   defining the regular columns one can define alternative sets of
   columns and then occasionally write out one of these alternative
   rows.  Alternative columns can be written interspersed with regular
   rows, and can be accessed with get_col just like regular ones
*)

type t

val dummy : ?non_path_vals:((string * string) list) ->
  string -> (string * string) list -> t

val get_altcol : t -> string -> float array

val get_keys : t -> string list

val print_df : t -> unit

val has_val : t -> string -> bool

val add_val : t -> string -> string -> unit

val get_val : t -> string -> string
  (** can raise Not_found *)

val get_all_val : t -> string -> string list
  (** can raise Not_found *)


val has_col : t -> string -> bool

val get_col : t -> string -> float array
  (** can raise Not_found *)

val get_name : t -> string

(******** reading an existing file ********)


val read : string -> in_channel -> t
  (** [read name ch] reads a datafile from [ch].  [name] is the name
      of the datafile... usually the path, but could be anything. *)

val load : ?check_trailer:bool -> string -> t

val seems_complete : string -> bool
val run_finished : string -> bool

(******** writing a new file ********)


type pairs = (string * string) list


val write_pairs : out_channel -> pairs -> unit

val write_colnames : out_channel -> string list -> unit
  (** it is expected that the rows will be written manually (as floats) *)

val write_alt_colnames : out_channel -> string -> string list -> unit
  (** first string is the name of this alternative set of columns *)

val write_alt_row_prefix : out_channel -> string -> unit
  (** tags this row as belonging to the alternative set of columns
      with the given name.  it is expected that the reminder of the
      row will be written manually (as floats) *)


val write_header_pairs : out_channel -> unit
  (** writes some standard pairs (start wall time, machine id) *)

val write_trailer_pairs : out_channel -> unit
  (** writes some standard pairs (wall finish time) *)


(**** high-level writing *****)


val pipe_data : in_channel -> out_channel -> (float * float)
  (** pipes from [inch] to [outch], flushing line-by-line and looking for
    "final sol cost" and "total raw cpu time" pairs, which are returned as a
    pair of floats after EOF *)


val call_solver : string -> string -> string -> pairs -> out_channel ->
  float * float
    (** calls external program [solver] on [args], piping in
	[prob_path], redirecting its output to [outch].  all logging
	is done within here.  returns cost, time pair *)


val do_run :  string -> string -> string -> string -> pairs -> unit
  (** sets up run file using [attrs] from [data_root] iff a complete
      one doesn't exist and calls [solver] with [args] and [prob_path]
      as input *)


(* EOF *)
