open Sig

module Make (A : ACCEPTOR) = struct
  include A
  include Basic.Make (A)

  module D = Det.Make (A)
  module O = Ops.Make (A)
  module M = Min.Brzozowski (A)

  module P = struct
    include O.P
    include D.P
    include M.P
  end
  module I = struct
    include O.I
    include D.I
    include M.I
  end
end

