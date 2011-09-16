open Sig

module type S = functor (A : ACCEPTOR) -> sig
  module P : sig
    val min : A.t -> A.t
  end
  module I : sig
    val min : A.t -> unit
  end
end

(* module Hopcroft (A : ACCEPTOR) : S *)

module Brzozowski : S

