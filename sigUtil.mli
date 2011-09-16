open Sig

module SIMPLE_BASIC_TYPE (X : ANY_TYPE) :
  BASIC_TYPE with type t = X.t

(** int as a basic type *)
module Int : BASIC_TYPE with type t = int
module Int32 : BASIC_TYPE with type t = int32
module Int64 : BASIC_TYPE with type t = int64
module Nativeint : BASIC_TYPE with type t = nativeint


(** Functor to make an ordered option type from an ordered type *)
module ORDERED_OPTION (X : ORDERED_TYPE) :
  ORDERED_TYPE with type t = X.t option

(** Functor to make a hashable option type from a hashable type *)
module HASHABLE_OPTION (X : HASHABLE_TYPE) :
  HASHABLE_TYPE with type t = X.t option

(** Functor to make a basic option type from a basic type *)
module BASIC_OPTION (X : BASIC_TYPE) :
  BASIC_TYPE with type t = X.t option

(** Functor to make an ordered pair type from two ordered types *)
module ORDERED_PAIR (X : ORDERED_TYPE) (Y : ORDERED_TYPE) :
  ORDERED_TYPE with type t = X.t * Y.t

(** Functor to make a hashable pair type from two hashable types *)
module HASHABLE_PAIR (X : HASHABLE_TYPE) (Y : HASHABLE_TYPE) :
  HASHABLE_TYPE with type t = X.t * Y.t

(** Functor to make a basic pair type from two basic types *)
module BASIC_PAIR (X : BASIC_TYPE) (Y : BASIC_TYPE) :
  BASIC_TYPE with type t = X.t * Y.t

module ORDERED_TYPE_WITH_SET (X : ORDERED_TYPE) : sig
  include ORDERED_TYPE with type t = X.t
  module Set : Set.S with type elt = X.t
end

module BASIC_TYPE_WITH_SET_AND_HASHTABLE (X : BASIC_TYPE) : sig
  include BASIC_TYPE with type t = X.t
  module Set : Set.S with type elt = X.t
  module Hashtbl : Hashtbl.S with type key = X.t
end
