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

let rec iter_ pre post it m st =
  let g (lbl, dst) = if pre (lbl, dst) then (iter_ pre post it m dst; post (lbl, dst)) in
    it g m st

let rec prefix_ f it m st =
  let g (lbl, dst) = if f (lbl, dst) then prefix_ f it m dst in
  it g m st

module DepthFirst (A : A_SUCC) = struct

  let iter ?post pre m st =
    match post with
    | None ->
       prefix_ pre A.iter_succ m st
    | Some post ->
      iter_ pre post A.iter_succ m st
  
end

module RevDepthFirst (A : A_PRED) = struct

  let iter ?post pre m st =
    match post with
    | None ->
      prefix_ pre A.iter_pred m st
    | Some post ->
      iter_ pre post A.iter_pred m st

end
