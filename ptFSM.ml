open Sig

module Make (Symbol : ORDERED_TYPE) = struct

  type symbol = Symbol.t
  type state = int
  
  
  module State = struct
    include SigUtil.Int
    module Set = struct
      include Ptset
      let hash = Hashtbl.hash
    end
    module Map = Ptmap
    module Hashtbl = Hashtbl.Make (SigUtil.Int)
  end
  module Symbol = struct
    include Symbol
    module Set = Set.Make (Symbol)
    module Map = Map.Make (Symbol)
  end
  
  type t = {
    mutable succ : State.Set.t Symbol.Map.t State.Map.t;
    mutable pred : State.Set.t Symbol.Map.t State.Map.t;
    mutable initial : State.Set.t ;
    mutable final : State.Set.t;
  }

  let create ?(size=20) () = 
    {
      succ = State.Map.empty;
      pred = State.Map.empty;
      initial = State.Set.empty;
      final = State.Set.empty;
    }

  let clear a =
    a.succ <- State.Map.empty;
    a.pred <- State.Map.empty;
    a.initial <- State.Set.empty;
    a.final <- State.Set.empty

  let add_tbl tbl a b c =
    if State.Map.mem a tbl then
      let m = State.Map.find a tbl in
      let s =
        if Symbol.Map.mem b m then
          State.Set.add c (Symbol.Map.find b m)
        else
          State.Set.singleton c
      in
        State.Map.add a (Symbol.Map.add b s m) tbl
    else
      State.Map.add a (Symbol.Map.add b (State.Set.singleton c) Symbol.Map.empty) tbl


  let add m (source, label, sink) =
    m.succ <- add_tbl m.succ source label sink;
    m.pred <- add_tbl m.pred sink label source

  let remove_tbl tbl (a : state) (b : symbol) (c : state) =
    if State.Map.mem a tbl then
      let m = State.Map.find a tbl in
      if Symbol.Map.mem b m then
        let s = Symbol.Map.find b m in
        if State.Set.mem c s then
          let m =
            let s = State.Set.remove c s in 
            if State.Set.is_empty s then
              Symbol.Map.remove b m
            else
              Symbol.Map.add b s m
          in
          if Symbol.Map.is_empty m then
            State.Map.remove a tbl
          else
            State.Map.add a m tbl
        else
          tbl
      else
        tbl
    else
      tbl

  let remove m (source, label, sink) =
    m.succ <- remove_tbl m.succ source label sink;
    m.pred <- remove_tbl m.pred sink label source

  let mem_tbl tbl (a : state) (b : symbol) (c : state) =
    State.Map.mem a tbl &&
      let m = State.Map.find a tbl in
    Symbol.Map.mem b m &&
      let s = Symbol.Map.find b m in
    State.Set.mem c s

  let mem m (source, label, sink) =
    mem_tbl m.succ source label sink

  let iter f a =
    let f src lbl dst = f (src, lbl, dst) in
    let f src lbl = State.Set.iter (f src lbl) in
    let f src = Symbol.Map.iter (f src) in
    State.Map.iter f a.succ

  let fold f a acc =
    let f src lbl dst acc = f (src, lbl, dst) acc in
    let f src lbl s acc = State.Set.fold (f src lbl) s acc in
    let f src m acc = Symbol.Map.fold (f src) m acc in
    State.Map.fold f a.succ acc

  let initial m = m.initial
  let set_initial m s = m.initial <- s
  let is_initial m st = State.Set.mem st m.initial
  let add_initial m st = m.initial <- State.Set.add st m.initial

  let final m = m.final
  let set_final m s = m.final <- s
  let is_final m st = State.Set.mem st m.final
  let add_final m st = m.final <- State.Set.add st m.final

  let states a = 
    let f src _m acc = 
      State.Set.add src acc
    in
    State.Map.fold f a.succ (State.Set.union a.initial a.final)
  let remove_state a src =
    if State.Map.mem src a.succ then
      let m = State.Map.find src a.succ in
      a.succ <- State.Map.remove src a.succ;
      let f lbl dst = a.pred <- remove_tbl a.pred dst lbl src in
      let f lbl s = State.Set.iter (f lbl) s in
      Symbol.Map.iter f m

  let rev m =
    { pred = m.succ ; succ = m.pred ; final = m.initial ; initial = m.final }
  
  let reverse m =
    let tmp = m.pred in m.pred <- m.succ; m.succ <- tmp;
    let tmp = m.initial in m.initial <- m.final; m.final <- tmp

  let copy m =
    {
      pred = m.pred;
      succ = m.succ;
      initial = m.initial;
      final = m.final;
    }
    
  let has_tbl h st =
    State.Map.mem st h &&
      not (Symbol.Map.is_empty (State.Map.find st h))

  let has_succ m st =
    has_tbl m.succ st

  let has_pred m st =
    has_tbl m.pred st

  let iter_tbl f h st =
    if State.Map.mem st h then
      let f lbl dst = f (lbl,dst) in
      let f lbl s = State.Set.iter (f lbl) s in
      Symbol.Map.iter f (State.Map.find st h)

  let iter_succ f m st =
    iter_tbl f m.succ st

  let iter_pred f m st =
    iter_tbl f m.pred st

  let fold_tbl f h st acc =
    if State.Map.mem st h then
      let f lbl dst = f (lbl,dst) in
      let f lbl s = State.Set.fold (f lbl) s in
      Symbol.Map.fold f (State.Map.find st h) acc
    else
      acc
  
  let fold_succ f m st acc =
    fold_tbl f m.succ st acc

  let fold_pred f m st acc =
    fold_tbl f m.pred st acc

  let get_sym h st sym =
    if State.Map.mem st h then
      let m = State.Map.find st h in
      if Symbol.Map.mem sym m then
        Symbol.Map.find sym m
      else
        State.Set.empty
    else
      State.Set.empty

  let delta a =
    get_sym a.succ

  let revdelta a =
    get_sym a.pred
    
  let size m =
    State.Map.cardinal m.succ

  let cannibalize m m' =
    m.pred <- m'.pred;
    m.succ <- m'.succ;
    m.initial <- m'.initial;
    m.final <- m'.final

end
