open Sig

module Make (A : ACCEPTOR) : sig
  include Sig.ACCEPTOR with type state = A.state and type symbol = A.symbol
  
  (** Find the set of states reachable from a set of states (by default the initial states) *)
  val reachable : ?from:State.Set.t -> t -> State.Set.t

  (** Find the set of states co-reachable from a set of states (by default the final states) *)
  val coreachable : ?from:State.Set.t -> t -> State.Set.t
  
  (** Find the useful states *)
  val useful : t -> State.Set.t
  
  (** Follow a path from a set of states. Returns the set of arrival states *)
  val walk : t -> State.Set.t -> symbol list -> State.Set.t
  
  (** Follow a path from a set of states, in reverse flow.
    Returns the set of arrival states *)
  val revwalk : t -> State.Set.t -> symbol list -> State.Set.t
  
  (** Check if a path is matched by an automaton *)
  val matches : t -> symbol list -> bool

  (** Non modifiying operations *)
  module P : sig
    (** Compute a trimmed automaton *)
    val trim : t -> t
    (** Union *)
    val union : t -> t -> t
    (** Intersection *)
    val inter : t -> t -> t
    (** Concatenation *)  
    val concat : t -> t -> t
    
    (** Determinization *)
    val det : t -> t

    (** Minimization *)
    val min : t -> t
  end
  
  (** In-place operations *)
  module I : sig
    (** Trim an automaton, i.e. keep only useful states *)
    val trim : t -> unit
    (** Union *)
    val union : t -> t -> unit
    (** Intersection *)
    val inter : t -> t -> unit
    (** Concatenation *)
    val concat : t -> t -> unit

    (** Determinization *)
    val det : t -> unit

    (** Minimization *)
    val min : t -> unit
  end
end
