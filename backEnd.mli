open Core_kernel.Std
open Types

val cprog_to_iprog: cprog -> iprog

val lift_constant_aliases: iprog -> iprog
val inline_constant_aliases: iprog -> iprog

(* Output format *)

type rprog = 
  string list * (string * string list) list

val rprog_of_iprog : prefix:string -> iprog -> rprog

val print_to_stdout : rprog -> unit
