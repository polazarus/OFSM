open Sig

module Make (A : ACCEPTOR) = struct
  module S = A.State.Set
  open A
  
  module DP = Traversal.DepthFirst (A)
  module RevDP = Traversal.RevDepthFirst (A)
  
  let reach_aux a traverse start =
    let set = ref start in
    let add (_lbl,st) = if S.mem st !set then false else (set := S.add st !set; true) in
    S.iter (traverse add a) start;
    !set


  let reachable ?from a =
    let from = match from with
      | None -> initial a
      | Some s -> s
    in
    reach_aux a DP.iter from
  let coreachable ?from a =
    let from = match from with
      | None -> final a
      | Some s -> s
    in
    reach_aux a RevDP.iter from
  let useful a =
    let reach = reachable a in
    let coreach = coreachable a in
    S.inter reach coreach
  
  let rec walk_aux delta start path =
  match path with
  | [] -> start
  | head :: tail ->
    let step = S.fold (fun start acc -> S.union (delta start head) acc) start S.empty in
    walk_aux delta step tail
  
  let walk a start path =
    walk_aux (delta a) start path
      
  let revwalk a start path =
    walk_aux (revdelta a) start path
  
  let matches a path =
    let e = walk a (initial a) path in
    not (S.is_empty (S.inter e (final a)))

end
