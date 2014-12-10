open Core.Std

(* A decision tree
 * The test results are the array indices *)
type ('a, 'n) t = 
  | Leaf of 'a
  | Test of 'n * ('a, 'n) t array

val fold : ('a, 'n) t -> leaf:('a -> 'b) -> test:('n -> 'b array -> 'b) -> 'b

val test1 : 'n -> 'a array -> ('a, 'n) t

val find_exn : ('a, 'n) t -> ('n * int) list -> 'a

val normalize :  ('a, 'n) t ->  ('a, 'n) t

val merge_exn :  ('a, 'n) t list ->  ('a list, 'n) t

val variables :
  comparator: ('n, 'w) Comparator.t ->
  ('a, 'n) t ->
  ('n, 'w) Set.t

include Monad.S2 with type ('a, 'n) t := ('a, 'n) t

