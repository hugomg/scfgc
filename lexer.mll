{
open Parser
open Lexing
open Core_kernel.Std

exception LexerError of string

(* Call this on all actions that might lex newlines *)
let update_line_number lexbuf = 
  String.iter (Lexing.lexeme lexbuf) ~f:(fun c ->
      if c = '\n' then Lexing.new_line lexbuf
    )

}

let newline = '\r'?'\n'
let whitespace =  [' ' '\t' '\r' '\n']
let alpha = ['A'-'Z' 'a'-'z']
let num   = ['0'-'9']

(* Valid names for variables, aliases and macros *)
let identifier = ['+''-']?(alpha|'_')(alpha|num|'_')*

(* Characters we can use in unquoted strings
 * "/" is not on the list because it conflicts with the "//" comment syntax *)
let unquoted = (alpha|num|'_'|'+'|'-'|'$')

(* Characters that can appear in quoted strings *)
let quoted = [^'"''\n']

rule lex_token tokenbuf = parse
  | newline { update_line_number lexbuf; NEWLINE }
  | whitespace { lex_token tokenbuf lexbuf }

  | "//" { do_comment lexbuf }
  
  | "alias"     { ALIAS }
  | "bind"      { BIND }
  | "define"    { DEFINE }
  | "var"       { VAR }
  | "switch"    { SWITCH }
  | "when"      { WHEN }
  | "increment" { INCREMENT }

  | '{'  { LBRACE } 
  | '}'  { Queue.enqueue tokenbuf RBRACE; NEWLINE }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | ','  { COMMA }
  | ':'  { COLON }
  | ';'  { SEMICOLON }
  | "->" { RARROW }
  | ":=" { ASSIGN }
  | "_"  { WILDCARD}

  | identifier as s
      { IDENTIFIER s }

  | unquoted+ as s
      { UNQUOTED s }
  | '"' (quoted* as s) '"'
      { QUOTED s }
  | '"' quoted*
      {
        update_line_number lexbuf;
        raise (LexerError "Unterminated string")
      }

  | eof { EOF }
  | _ as c { raise (LexerError (sprintf "Unquoted '%c'" c)) }


and do_comment = parse
  | eof { EOF }
  | newline { update_line_number lexbuf; NEWLINE }
  | _ { do_comment lexbuf }

{

(* I want to make semicolons optional before a closing brace but I don't want
 * to have the semicolons be a statement separator, like in Ocaml or Pascal,
 * because that would require semicolons after if statements. So what I do is
 * hack the lexer so closing braces return a statement terminator token in addition to
 * the close-brace token *)
let create () =
  let buf = Queue.create () in
  fun lexbuf ->
    match Queue.dequeue buf with
    | None -> lex_token buf lexbuf
    | Some tk -> tk
}
