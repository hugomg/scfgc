open Core_kernel.Std

module type S = sig
  type t with sexp
  val to_int : t -> int
  val fresh : unit -> t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Make ( ) : S = struct
  type t = int with sexp
  let to_int x = x

  let next = ref 0
  let fresh () =
    let id = !next in
    next := id + 1;
    id

  include Comparable.Make(Int)
  include Hashable.Make(Int)
end
