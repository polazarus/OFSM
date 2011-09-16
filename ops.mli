open Sig

module Make (A : ACCEPTOR) : sig
  open A

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
  end

end
