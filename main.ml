open Core.Std

open Lexing
open Types

let print_sexp sx =
  print_endline (Sexp.to_string_hum ~indent:2 sx)

let print_prog parse_tree = 
  parse_tree |>
  sexp_of_parse_tree |>
  print_sexp

let print_cprog cprog = 
  print_sexp (sexp_of_cprog cprog)

let print_iprog iprog = 
  let rprog = BackEnd.rprog_of_iprog ~main:"main" ~prefix:"ss" iprog in
  BackEnd.print_to_stdout rprog

let () =
  if Array.length Sys.argv <= 1 then (
    printf "usage: %s FILE\n" Sys.argv.(0);
    exit 1
  );

  let filearg = Sys.argv.(1) in
  
  let filename, chan =
    if filearg = "-" then
      ("**stdin**", In_channel.stdin)
    else
      (filearg, In_channel.create filearg)
  in

  let buf = Lexing.from_channel chan in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };

  let onerror errname pos errinfo= 
    printf "Line %d, column %d: %s" (pos_lnum pos) (pos_cnum pos) errname;
    match errinfo with
    | Some s -> printf "\n%s\n" s
    | None -> printf "\n"
  in

  try
    let parse_tree = Parser.prog Lexer.token buf in
(*    print_endline "PARSE =============";*)
(*    print_prog parse_tree;*)
    let cprog = FrontEnd.parse_tree_to_cprog parse_tree in
    print_endline "CPROG =============";
    print_cprog cprog;
    let iprog = BackEnd.cprog_to_iprog cprog in
    print_endline "IPROG =============";
    print_iprog iprog;
    let iprog = BackEnd.inline_constant_aliases iprog in
    print_endline "INLINE =============";
    print_iprog iprog;
    let iprog = BackEnd.lift_constant_aliases iprog in
    print_endline "LIFT =============";
    print_iprog iprog;
    ()
  with
    | Lexer.LexerError s ->
        onerror "Lexer Error" buf.lex_curr_p (Some s)
    | Parser.Error ->
        onerror "Syntax Error" buf.lex_curr_p None
    | CompilationError(errs) ->
        List.iter errs ~f:(fun (pos, msg) ->
          onerror "Error" pos (Some msg))


