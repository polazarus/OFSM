open Sig

module Make (A : ACCEPTOR) : sig
  module P : sig
    val det : A.t -> A.t
  end
  module I : sig
    val det : A.t -> unit
  end
end
