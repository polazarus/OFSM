open Sig

module Make (A : ACCEPTOR) : sig
  open A

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
end
