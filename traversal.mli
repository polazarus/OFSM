module type A_SUCC = sig
  type t
  type symbol
  type state
  val iter_succ : (symbol*state -> unit) -> t -> state -> unit
end

module type A_PRED = sig
  type t
  type symbol
  type state
  val iter_pred : (symbol*state -> unit) -> t -> state -> unit
end

module DepthFirst (A : A_SUCC) : sig
  val iter : ?post:(A.symbol * A.state -> unit) -> (A.symbol * A.state -> bool) -> A.t -> A.state -> unit
end
module RevDepthFirst (A : A_PRED) : sig
  val iter : ?post:(A.symbol * A.state -> unit) -> (A.symbol * A.state -> bool) -> A.t -> A.state -> unit
end
