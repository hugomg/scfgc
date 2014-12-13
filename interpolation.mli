open Core_kernel.Std

(* A datatype to represent strings with $xxx patterns that get substituted *)

type 'a t = I of string * (int * int * 'a) list

val map:
  f:('a -> 'b) ->
  'a t -> 'b t

val of_string: string -> string t
val to_string: string t -> string
