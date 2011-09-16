open Sig

module SIMPLE_BASIC_TYPE (T : ANY_TYPE) = struct
  include T
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Int = SIMPLE_BASIC_TYPE (struct type t = int end)
module Int32 = SIMPLE_BASIC_TYPE (struct type t = int32 end)
module Int64 = SIMPLE_BASIC_TYPE (struct type t = int64 end)
module Nativeint = SIMPLE_BASIC_TYPE (struct type t = nativeint end)

module ORDERED_OPTION (X : ORDERED_TYPE) = struct
  type t = X.t option

  let compare a b =
    match a, b with
    | None, None -> 0
    | None, _ -> (-1)
    | _, None -> 1
    | Some a, Some b -> X.compare a b
end

module HASHABLE_OPTION (X : HASHABLE_TYPE) = struct
  type t = X.t option

  let equal a b =
    match a,b with
    | Some a, Some b -> X.equal a b
    | None, None -> true
    | _ -> false

  let hash o =
    match o with
    | None -> 0
    | Some o -> X.hash o
end

module BASIC_OPTION (X : BASIC_TYPE) = struct
  type t = X.t option
  
  let compare a b =
    match a, b with
    | None, None -> 0
    | None, _ -> (-1)
    | _, None -> 1
    | Some a, Some b -> X.compare a b
  let equal a b =
    match a,b with
    | Some a, Some b -> X.equal a b
    | None, None -> true
    | _ -> false

  let hash o =
    match o with
    | None -> 0
    | Some o -> X.hash o
end

module ORDERED_PAIR (X : ORDERED_TYPE) (Y : ORDERED_TYPE) = struct
  type t = X.t * Y.t

  let compare (a,b) (a',b')=
    let c = X.compare a a' in
    if c == 0 then Y.compare b b' else c
end

module HASHABLE_PAIR (X : HASHABLE_TYPE) (Y : HASHABLE_TYPE) = struct
  type t = X.t * Y.t

  let equal (x1, y1) (x2, y2) =
    X.equal x1 x2 && Y.equal y1 y2

  let hash (x, y) =
    Hashtbl.hash (X.hash x, Y.hash y)
end

module BASIC_PAIR (X : BASIC_TYPE) (Y : BASIC_TYPE) = struct
  type t = X.t * Y.t

  let compare (a,b) (a',b')=
    let c = X.compare a a' in
    if c == 0 then Y.compare b b' else c

  let equal (x1, y1) (x2, y2) =
    X.equal x1 x2 && Y.equal y1 y2

  let hash (x, y) =
    Hashtbl.hash (X.hash x, Y.hash y)
end

module ORDERED_TYPE_WITH_SET (X : ORDERED_TYPE) = struct
  include X
  module Set = Set.Make (X)
end

module BASIC_TYPE_WITH_SET_AND_HASHTABLE (X : BASIC_TYPE) = struct
  include X
  module Set = Set.Make (X)
  module Hashtbl = Hashtbl.Make (X)
end
