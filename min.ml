open Sig

module type S = functor (A : ACCEPTOR) -> sig
  module P : sig
    val min : A.t -> A.t
  end
  module I : sig
    val min : A.t -> unit
  end
end

module Brzozowski (A : ACCEPTOR) = struct
  include A
  module D = Det.Make (A)
  open D

  module P = struct
    let min a =
      let b = rev a in
      D.I.det b;
      reverse b;
      D.I.det b;
      b
  end

  module I = struct
    let min a =
      reverse a;
      D.I.det a;
      reverse a;
      D.I.det a
  end
end
