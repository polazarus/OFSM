(** Standard signature *)

(** Any type *)
module type ANY_TYPE = sig
  type t
end

(** Any hashable type *)
module type HASHABLE_TYPE = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

(** Any ordered type *)
module type ORDERED_TYPE = sig
  type t
  val compare : t -> t -> int
end

(** Any hashable ordered type *)
module type BASIC_TYPE = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

(** Accceptor *)
module type ACCEPTOR = sig
  type t
  type state = int
  type symbol
  
  module State : sig
    include BASIC_TYPE with type t = int
    module Set : sig
      include Set.S with type elt = int
      val hash : t -> int
    end
    module Hashtbl : Hashtbl.S with type key = state
  end

  module Symbol : sig
    include ORDERED_TYPE with type t = symbol
    module Set : Set.S with type elt = symbol
  end

  val create : ?size:int -> unit -> t
  val clear : t -> unit
  
  (** Get the number of used states *)
  val size : t -> int
  
  val initial : t -> State.Set.t
  val set_initial : t -> State.Set.t -> unit
  val is_initial : t -> state -> bool
  val add_initial : t -> state -> unit

  val final : t -> State.Set.t
  val set_final : t -> State.Set.t -> unit
  val is_final : t -> state -> bool
  val add_final : t -> state -> unit

  val mem : t -> state*symbol*state -> bool
  val add : t -> state*symbol*state -> unit
  val remove : t -> state*symbol*state -> unit
  val iter : (state*symbol*state -> unit) -> t-> unit
  val fold : (state*symbol*state -> 'a -> 'a) -> t -> 'a -> 'a

  val states : t -> State.Set.t
  val remove_state : t -> state -> unit

  val has_succ : t -> state -> bool
  val has_pred : t -> state -> bool

  val iter_succ : (symbol * state -> unit) -> t -> state -> unit
  val iter_pred : (symbol * state -> unit) -> t -> state -> unit
  val fold_succ : (symbol * state -> 'a -> 'a) -> t -> state -> 'a -> 'a
  val fold_pred : (symbol * state -> 'a -> 'a) -> t -> state -> 'a -> 'a

  val delta : t -> state -> symbol -> State.Set.t
  val revdelta : t -> state -> symbol -> State.Set.t

  val rev : t -> t
  val reverse : t -> unit

  val cannibalize : t -> t -> unit
  val copy : t -> t

(*  module State = sig*)
(*    val mem : t -> state*)
(*    val iter : t -> (state -> unit) -> unit*)
(*    val fold : t -> (state -> 'a -> 'a) -> unit*)
(*  end*)
(*  *)
(*  module Succ = sig*)
(*    val add : t -> state -> symbol*state*)
(*    val mem : t -> state*)
(*    val iter*)

(*  module Pred = sig*)
(*    val add : t -> state -> symbol*state*)
(*    val mem : t -> state*)
(*    val iter : t -> state -> (state -> unit) -> unit*)
(*  end*)
(*  *)
(*  val iter_states : t -> ()*)
end
