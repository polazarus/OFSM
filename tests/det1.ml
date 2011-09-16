open OFSM


module FSM = Full.Make (HFSM.Make (String))

module Dot = Output.Dot (Output.SimpleDotParams) (FSM) (struct
  type t = string
  let to_string a = a
end)

let out f a =
  let oc = open_out (f^".dot") in
  let formatter = Format.formatter_of_out_channel oc in
  Dot.output formatter a;
  Format.pp_print_flush formatter ();
  close_out oc
(*  ignore (Unix.system ("dot -T png \'"^f^".dot\' -o \'"^f^".png\'"))*)

let make l i f =
  let a = FSM.create () in
    List.iter (FSM.add a) l;
    List.iter (FSM.add_initial a) i;
    List.iter (FSM.add_final a) f;
    a


let () =
  let a = make [1, "a", 2; 1, "a", 3; 2, "a", 2; 2, "b", 3] [1] [3] in
  let b = make [1, "a", 2; 1, "a", 3; 2, "b", 4; 3, "c", 4] [1] [4] in
  
(*  let c = make [1, "a", 2; 2, "b", 3; 3, "c", 2] [1] [2] in*)
(*  let d = make [1, "b", 2; 2, "c", 1; 2, "c", 3; 3, "d", 4] [1] [4] in*)
  out "det1_a" a;
  FSM.I.det a;
  out "det1_a_det" a;

  out "det1_b" b;
  FSM.I.det b;
  out "det1_b_det" b;

  ()
