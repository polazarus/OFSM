open Sig

module Make (A : ACCEPTOR) = struct
  open A
  
  module M = Map.Make (Symbol)
  module S = State.Set
  module H = Hashtbl.Make (S)

  let sort_succ_by_symbol a src mapAcc =
    let add_map (key, value) acc =
      let set =
        if M.mem key acc then S.add value (M.find key acc)
        else S.singleton value
      in
        M.add key set acc
    in
    fold_succ add_map a src mapAcc

  let sort_succ_by_symbol_and_check_final a srcset =
    let f src (finAcc,mapAcc) = (finAcc || is_final a src, sort_succ_by_symbol a src mapAcc) in
    S.fold f srcset (false, M.empty)

  module P = struct

    let det p =
      let init = initial p in
      if S.is_empty init then
        create ~size:1 ()
      else
        let size = size p in
        let res = create ~size () in
        let q = Queue.create () in
        let next_state =
          let next = ref 0 in
          fun _ -> let n = !next in incr next; n
        in
        let h = H.create size in
        let v = next_state () in
      
        Queue.push (v, init) q;
        add_initial res v;
      
        let add_edge src label dstset =
          let dst =
            if H.mem h dstset then
              H.find h dstset
            else
              let st = next_state () in
                H.add h dstset st;
                Queue.push (st,dstset) q;
                st
          in
            add res (src, label, dst)
        in
          while not (Queue.is_empty q) do
            let (v, set) = Queue.pop q in
            let fin,outgoings = sort_succ_by_symbol_and_check_final p set in
            if fin then add_final res v;
            M.iter (add_edge v) outgoings
          done;
          res
  end
  
  module I = struct

    let det p =
      let init = initial p in
      if S.is_empty init then
        clear p
      else
        let q = Queue.create () in
        let q_cloned = Queue.create () in
        let next_state =
          let next = ref (1 + S.max_elt (states p)) in
          fun _ -> let n = !next in incr next; n
        in
        let h = H.create (size p) in
        
        (* Get the replacing state for a set of states *)
        let state_for set =
          if S.cardinal set = 1 then
            S.choose set
          else begin
            S.iter (fun i -> Queue.push i q_cloned) set;
            next_state ()
          end
        in
        
        (* Add an edge between src and a set of states (by merging them) *)
        let add_edge src label dstset =
          let dst =
            if H.mem h dstset then
              H.find h dstset
            else
              let st = state_for dstset in
                H.add h dstset st;
                Queue.push (st,dstset) q;
                st
          in
            add p (src, label, dst)
        in

        let v = state_for init in
        H.add h init v;
        Queue.push (v,init) q;
        set_initial p (S.singleton v);
        
        (* Determinization *)
        while not (Queue.is_empty q) do
          let (v, set) = Queue.pop q in
          
          (* Sort outgoing from transitions from the original set of states *)
          let fin, outgoings = sort_succ_by_symbol_and_check_final p set in
          if fin then add_final p v;
          
          (* Remove any previous outgoing transitions *)
          iter_succ (fun (lbl,dst) -> remove p (v, lbl,dst)) p v;

          (* Add outgoing transitions*)
          M.iter (add_edge v) outgoings;
        done;

        (* Cleaning up cloned states *)
        while not (Queue.is_empty q_cloned) do
          let v = Queue.pop q_cloned in
          if not (has_pred p v) then begin
            iter_succ (fun (_lbl,dst) -> Queue.push dst q_cloned) p v;
            remove_state p v
          end
        done

  end

end
