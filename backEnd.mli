open Core.Std
open Types

val cprog_to_iprog: cprog -> iprog

val lift_constant_aliases: iprog -> iprog
val inline_constant_aliases: iprog -> iprog

(* Output format *)

type rprog = {
  r_prefix: string;
  r_files: (string * string list) list (* filename -> lines *)
}
                                                                                                   
val rprog_of_iprog : prefix:string -> main:string -> iprog -> rprog

val print_to_stdout : rprog -> unit
(*val print_to_files  : rprog -> unit*)
