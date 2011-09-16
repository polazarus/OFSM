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
    module Hashtbl = Hashtbl.Make (SigUtil.Int)
  end
  module Symbol = struct
    include Symbol
    module Set = Set.Make (Symbol)
    module Map = Map.Make (Symbol)
  end
  
  type t = {
    mutable succ : State.Set.t Symbol.Map.t State.Hashtbl.t;
    mutable pred : State.Set.t Symbol.Map.t State.Hashtbl.t;
    mutable initial : State.Set.t ;
    mutable final : State.Set.t;
  }

  let create ?(size=20) () = 
    {
      succ = State.Hashtbl.create size;
      pred = State.Hashtbl.create size;
      initial = State.Set.empty;
      final = State.Set.empty;
    }
  let clear a =
    State.Hashtbl.clear a.succ;
    State.Hashtbl.clear a.pred;
    a.initial <- State.Set.empty;
    a.final <- State.Set.empty

  let add_tbl tbl a b c =
      if State.Hashtbl.mem tbl a then
        let m = State.Hashtbl.find tbl a in
        let s =
          if Symbol.Map.mem b m then
            State.Set.add c (Symbol.Map.find b m)
          else
            State.Set.singleton c
        in
          State.Hashtbl.replace tbl a (Symbol.Map.add b s m)
      else
        State.Hashtbl.add tbl a (Symbol.Map.add b (State.Set.singleton c) Symbol.Map.empty)


  let add m (source, label, sink) =
    add_tbl m.succ source label sink;
    add_tbl m.pred sink label source

  let remove_tbl tbl (a : state) (b : symbol) (c : state) =
    if State.Hashtbl.mem tbl a then
      let m = State.Hashtbl.find tbl a in
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
            State.Hashtbl.remove tbl a
          else
            State.Hashtbl.replace tbl a m

  let remove m (source, label, sink) =
    remove_tbl m.succ source label sink;
    remove_tbl m.pred sink label source

  let mem_tbl tbl (a : state) (b : symbol) (c : state) =
    State.Hashtbl.mem tbl a &&
      let m = State.Hashtbl.find tbl a in
    Symbol.Map.mem b m &&
      let s = Symbol.Map.find b m in
    State.Set.mem c s

  let mem m (source, label, sink) =
    mem_tbl m.succ source label sink

  let iter f a =
    let f src lbl dst = f (src, lbl, dst) in
    let f src lbl = State.Set.iter (f src lbl) in
    let f src = Symbol.Map.iter (f src) in
    State.Hashtbl.iter f a.succ

  let fold f a acc =
    let f src lbl dst acc = f (src, lbl, dst) acc in
    let f src lbl s acc = State.Set.fold (f src lbl) s acc in
    let f src m acc = Symbol.Map.fold (f src) m acc in
    State.Hashtbl.fold f a.succ acc

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
    State.Hashtbl.fold f a.pred
      (State.Hashtbl.fold f a.succ
        (State.Set.union a.initial a.final))

  let remove_state a st =
    begin if State.Hashtbl.mem a.succ st then
      let m = State.Hashtbl.find a.succ st in
      State.Hashtbl.remove a.succ st;
      let f lbl other = remove_tbl a.pred other lbl st in
      let f lbl s = State.Set.iter (f lbl) s in
      Symbol.Map.iter f m
    end;
    begin if State.Hashtbl.mem a.pred st then
      let m = State.Hashtbl.find a.pred st in
      State.Hashtbl.remove a.pred st;
      let f lbl other = remove_tbl a.succ other lbl st in
      let f lbl s = State.Set.iter (f lbl) s in
      Symbol.Map.iter f m
    end;
    if State.Set.mem st a.final then a.final <- State.Set.remove st a.final;
    if State.Set.mem st a.initial then a.initial <- State.Set.remove st a.initial

  let rev m =
    { pred = m.succ ; succ = m.pred ; final = m.initial ; initial = m.final }
  
  let reverse m =
    let tmp = m.pred in m.pred <- m.succ; m.succ <- tmp;
    let tmp = m.initial in m.initial <- m.final; m.final <- tmp

  let copy m =
    {
      pred = State.Hashtbl.copy m.pred;
      succ = State.Hashtbl.copy m.succ;
      initial = m.initial;
      final = m.final;
    }

  let iter_tbl f h st =
    if State.Hashtbl.mem h st then
      let f lbl dst = f (lbl,dst) in
      let f lbl s = State.Set.iter (f lbl) s in
      Symbol.Map.iter f (State.Hashtbl.find h st)

  let iter_succ f m st =
    iter_tbl f m.succ st

  let iter_pred f m st =
    iter_tbl f m.pred st

  let has_tbl h st =
    State.Hashtbl.mem h st &&
      not (Symbol.Map.is_empty (State.Hashtbl.find h st))

  let has_succ m st =
    has_tbl m.succ st

  let has_pred m st =
    has_tbl m.pred st

  let fold_tbl f h st acc =
    if State.Hashtbl.mem h st then
      let f lbl dst = f (lbl,dst) in
      let f lbl s = State.Set.fold (f lbl) s in
      Symbol.Map.fold f (State.Hashtbl.find h st) acc
    else
      acc
  
  let fold_succ f m st acc =
    fold_tbl f m.succ st acc

  let fold_pred f m st acc =
    fold_tbl f m.pred st acc

  let get_sym h st sym =
    if State.Hashtbl.mem h st then
      let m = State.Hashtbl.find h st in
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
    State.Hashtbl.length m.succ

  let cannibalize m m' =
    m.pred <- m'.pred;
    m.succ <- m'.succ;
    m.initial <- m'.initial;
    m.final <- m'.final

end
