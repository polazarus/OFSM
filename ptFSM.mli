open Sig

module Make (Symbol : ORDERED_TYPE) : ACCEPTOR
  with type symbol = Symbol.t
