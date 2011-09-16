open Sig
open SigUtil

module Make (A : ACCEPTOR) = struct
  open A

  module B = Basic.Make (A)
  open B

  module M = Map.Make (Symbol)
  
  module S = struct
    include State.Set
    let hash s = State.Set.fold (fun e acc -> Hashtbl.hash (State.hash e,acc)) s 0
  end
  module H = Hashtbl.Make (HASHABLE_PAIR (S) (S))
  

  let add_left a1 =
    S.fold (fold_succ (fun (lbl,dst) acc ->
      let nSet =
        if M.mem lbl acc then
          let (o1, o2), (f1, f2) = M.find lbl acc in
          ((S.add dst o1, o2), (f1 || is_final a1 dst, f2))
        else
          ((S.singleton dst, S.empty), (is_final a1 dst, false))
      in
      M.add lbl nSet acc
    ) a1)

  let add_right a2 =
    S.fold (fold_succ (fun (lbl,dst) acc ->
      let nSet =
        if M.mem lbl acc then
          let (o1, o2), (f1, f2) = M.find lbl acc in
          ((o1, S.add dst o2), (f1, f2 || is_final a2 dst))
        else
          ((S.empty, S.singleton dst), (false, is_final a2 dst))
      in
      M.add lbl nSet acc
    ) a2)

  module P = struct

  let union a1 a2 =
    let size = (size a1) + (size a2) in
    let ares = create ~size () in
    let q = Queue.create () in
    let new_state_for = 
      let h : int H.t = H.create size in
      let n = ref (-1) in
      fun old ->
        if H.mem h old then
          H.find h old
        else begin
          incr n;
          let n = !n in
            H.add h old n;
            Queue.add (n, old) q;
            n
        end
    in
    let init1 = initial a1 and init2 = initial a2 in
    if not (S.is_empty init1 && S.is_empty init2) then begin
      let initres = new_state_for (init1, init2) in
      add_initial ares initres;
      if not (S.is_empty (S.inter init1 (final a1))) ||
        not (S.is_empty (S.inter init2 (final a2))) then
        add_final ares initres;

      while not (Queue.is_empty q) do
        let (v, (s1, s2)) = Queue.pop q in
        let m = add_left a1 s1 M.empty in
        let m = add_right a2 s2 m in
        M.iter (fun lbl (sets,(f1,f2)) ->
          let v' = new_state_for sets in
          if f1 || f2 then add_final ares v';
          add ares (v,lbl,v')
        ) m;
      done;
    end;
    ares

    let trim a =
      let u = useful a in
      let ares = create ~size:(S.cardinal u) () in
      set_initial ares (S.inter u (initial a));
      set_final ares (S.inter u (final a));
      iter (fun (src,lbl,dst) ->
        if S.mem src u && S.mem dst u then
          add ares (src,lbl,dst)
      ) a;
      ares
      
  
    let inter a1 a2 =
      let size = size a1 in
      let ares = create ~size () in
      let q = Queue.create () in
      let new_state_for = 
        let h : int H.t = H.create size in
        let n = ref (-1) in
        fun old ->
          if H.mem h old then
            H.find h old
          else begin
            incr n;
            let n = !n in
              H.add h old n;
              Queue.add (n, old) q;
              n
          end
      in
      let init1 = initial a1 and init2 = initial a2 in
      if not (S.is_empty init1 || S.is_empty init2) then begin
        let initres = new_state_for (init1, init2) in
        add_initial ares initres;
        if not (S.is_empty (S.inter init1 (final a1))) ||
          not (S.is_empty (S.inter init2 (final a2))) then
          add_final ares initres;

        while not (Queue.is_empty q) do
          let (v, (s1, s2)) = Queue.pop q in
          let m = add_left a1 s1 M.empty in
          let m = add_right a2 s2 m in
          M.iter (fun lbl ((s1,s2) as sets,(f1,f2)) ->
            if not (S.is_empty s1 || S.is_empty s2) then
              let v' = new_state_for sets in
              if f1 && f2 then add_final ares v';
              add ares (v,lbl,v')
          ) m;
        done;
      end;
      ares

    let concat a1 a2 =
      let size = size a1 + size a2 in
      let ares = create ~size () in
      let q = Queue.create () in
      let new_state_for = 
        let h : int H.t = H.create size in
        let n = ref (-1) in
        fun old ->
          if H.mem h old then
            H.find h old
          else begin
            incr n;
            let n = !n in
              H.add h old n;
              Queue.add (n, old) q;
              n
          end
      in
      let init1 = initial a1 and init2 = initial a2 in
      let init_final1 = not (S.is_empty (S.inter init1 (final a1))) in
      let init_final2 = not (S.is_empty (S.inter init2 (final a2))) in

      if not (S.is_empty init1 || S.is_empty init2) then begin
        let s2 = if init_final1 then init2 else S.empty in
  (*      Printf.eprintf "init_final1=%b init_final2=%b \n" init_final1 init_final2;*)
        let initres = (new_state_for (init1, s2)) in
        add_initial ares initres;
        if init_final1 && init_final2 then
          add_final ares initres;
        
        
        while not (Queue.is_empty q) do
          let (v, (s1, s2)) = Queue.pop q in
          let m = add_left a1 s1 M.empty in
          let m = add_right a2 s2 m in
          M.iter (fun lbl ((s1,s2),(f1, f2)) ->
            let s2,f2 = if f1 then (S.union s2 init2, f2 || init_final2) else (s2,f2) in
            let v' = new_state_for (s1, s2) in
            if f2 then add_final ares v';
            add ares (v,lbl,v')
          ) m;
        done;
      end;
      ares
  end


  module I = struct
    let trim a =
      let all = states a in
      let useful = useful a in
      let toremove = S.diff all useful in
      S.iter (remove_state a) toremove

    let union a1 a2 = cannibalize a1 (P.union a1 a2)
    let inter a1 a2 = cannibalize a1 (P.inter a1 a2)
    let concat a1 a2 = cannibalize a1 (P.concat a1 a2)
  end
end
