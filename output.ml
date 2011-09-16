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

module SimpleDotParams = struct
  let node_attrs = "shape=point"
  let initial_attrs = "color=red"
  let final_attrs = "color=blue"
  let both_attrs = "color=purple"
end

module Dot (P : DOT_PARAMS) (A : ACCEPTOR with type state = int) (SymbolToString : TO_STRING with type t = A.symbol) = struct
  open A
  type t = A.t
  open P
  
  let output
      formatter a =
    let module S = State.Set in
    Format.fprintf formatter "@[<v 2>digraph {";
    Format.fprintf formatter "@,@[node [%s];@]" node_attrs;
    let i = initial a and f = final a in
    let inter = S.inter i f in
    let i = S.diff i inter and f = S.diff f inter in
    S.iter (fun i -> Format.fprintf formatter "@,@[%d [%s]; @]" i initial_attrs) i;
    S.iter (fun i -> Format.fprintf formatter "@,@[%d [%s]; @]" i final_attrs) f;
    S.iter (fun i -> Format.fprintf formatter "@,@[%d [%s]; @]" i both_attrs) inter;
    iter (fun (src, lbl, dst) ->
      Format.fprintf formatter "@,@[%d -> %d [label=\"%s\"];@]" src dst (SymbolToString.to_string lbl)
    ) a;
    Format.fprintf formatter "@]@,}@.";;

end
