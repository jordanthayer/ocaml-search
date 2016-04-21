(** A packed array of ints.  Each element is stored as a subset of the
    total number of bits in an int.  Elements are not optimally
    packed.  This means that if they don't fit evenly in an int there
    may be unused, leftover bits in each int.

    The reason for the [functions] interface is so that all of the
    overhead information (bits per element, elements per int, etc.) is
    stored in closures and not tagging along with each array.  This
    reduces the amount of memory used for each individual array.

    @author eaburns
    @since 2010-08-31
*)

type t = int array

type info

type create_fun = int -> t

type get_fun = t -> int -> int

type set_fun = t -> int -> int -> unit


val info : int -> info
  (** [info elm_max] makes an info record for packed ints with a given
      size element. *)


val make_create : info -> int -> t
  (** [create info] makes function that makes a new packed array with
      [nelms] elements. *)


val make_get : info -> t -> int -> int
  (** [make_get info] makes a function that gets the [i]th element of
      [ary]. *)


val make_set : info -> t -> int -> int -> unit
  (** [make_set info] makes a function that sets the [i]th element of
      [ary] to be [vl]. *)


val copy : t -> t
  (** [copy a] copies [a]. *)


val make_mapi : info -> int -> (int -> int -> int) -> t -> t
  (** [make_mapi info nelms f ary] maps [f] over the array [ary] which
      has [nelms] elements. [f] is also passed the index. *)


val make_map : info -> int -> (int -> int) -> t -> t
  (** [make_map info nelms f ary] maps [f] over the array [ary] which
      has [nelms] elements. *)


val make_fold_left :
  info -> int -> ('a -> int -> 'a) -> 'a -> t -> 'a
  (** [make_fold_left info nelms f init ary] folds [f] leftwards over
      the array [ary] which has [nelms] elements. *)


val make_fold_lefti :
  info -> int -> ('a -> int -> int -> 'a) -> 'a -> t -> 'a
  (** [make_fold_left info nelms f init ary] folds [f] leftwards over
      the array [ary] which has [nelms] elements. *)

val make_iter : info -> int -> (int -> unit) -> t -> unit
  (** [make_iter info nelms f ary] iterates [f] over the array [ary]
      which has [nelms] elements. *)


val make_iteri : info -> int -> (int -> int -> unit) -> t -> unit
  (** [make_iter info nelms f ary] iterates [f] over the array [ary]
      which has [nelms] elements. *)

val unpack : t -> int -> int array
  (**  turns t back into an int array.  *)
