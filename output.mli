open Sig

module type TO_STRING = sig
  type t
  val to_string : t -> string
end

module type OUTPUT = functor (A : ACCEPTOR) -> functor (T : TO_STRING with type t = A.symbol) -> sig
  val output : Format.formatter -> A.t -> unit
end

module type DOT_PARAMS = sig
  val node_attrs : string
  val initial_attrs : string
  val final_attrs : string
  val both_attrs : string
end

module SimpleDotParams : DOT_PARAMS

module Dot (D : DOT_PARAMS) : OUTPUT
