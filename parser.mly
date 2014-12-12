%{
open Types
open Either
%}

(* keywords *)
%token ALIAS
%token BIND
%token DEFINE
%token VAR
%token SWITCH
%token WHEN
%token INCREMENT
(* cmds *)
%token <string> IDENTIFIER
%token <string> UNQUOTED
%token <string> QUOTED
(* symbols *)
%token LBRACE RBRACE
%token LPAREN RPAREN
%token COMMA COLON SEMICOLON
%token RARROW
%token ASSIGN
%token WILDCARD
(* soft statement terminator *)
%token NEWLINE
(* EOF *)
%token EOF

%start prog

%type <Types.parse_tree> prog
%%


(* Making newlines act as command terminators is useful
 * for backwards-compatibility and bugs: forgetting a semicolon
 * would cause the command in the second line to be treated as 
 * parameters to the command in the previous line. 
 *
 * However, the downside of having newlines produce a token instead of being
 * eaten by the lexer is that now we need to find a way to ignore those newline
 * tokens everywhere thats not the end of a statement. Unfortunatelly, the 
 * inly way I could think to do this involves putting a call to "s" after almost
 * every non terminal in the grammar, which is very tricky to get right and prone
 * to forgetting about some cases :/
 *)
s:
  | (*empty*) {()}
  | s NEWLINE {()}

terminator:
  | NEWLINE s {()}
  | SEMICOLON s {()}

prog: 
  | s b=block EOF { b }

block:
  | stmts_or_decls=list(stmt_or_decl) {
      (* We wrap extra "Block" stmts around declarations that are mixed
       * with other statements instead of being at the top of the block *)
      let open Core.Std in
      let to_block pos xs =
        match xs with
        | [x] -> x (* no need for a Block node when there is a single stmt *)
        | _ -> ParseTree(pos, Block xs)
      in
      let nested_decls = 
        List.fold_right stmts_or_decls
          ~init: []
          ~f:(fun x rest ->
              match x with
              | Left (pos, decl) -> [ ParseTree(pos, decl (to_block pos rest)) ]
              | Right stmt -> stmt :: rest
            )
      in
      to_block $startpos nested_decls }

stmt_or_decl: 
  | decl { Left $1 }
  | stmt { Right $1 }


decl: 
  | raw_decl { ($startpos, $1) }


raw_decl:
  | DEFINE s name=name s value=expr terminator
      { fun scope -> Let(name, Decl_Const(value), scope) }
  | DEFINE s
    name=name s
    LPAREN s 
    args=separated_list(terminated(COMMA, s), terminated(name, s))
    RPAREN s
    body=stmt
      { fun scope -> Let(name, Decl_Macro(args, body), scope) }
  | ALIAS s name=alias_name s body=stmt
      { fun scope -> Let(name, Decl_Alias(body), scope) }
  | VAR s
    name=name s
    COLON s
    LBRACE s
    domain=separated_list(terminated(COMMA, s), terminated(str, s))
    RBRACE (*no s*)
    m_init=ioption(varinit) terminator
      { 
        let open Core.Std in
        match domain with
        | [] -> raise (CompilationError([
            ($startpos(domain), "Empty variable domain")
          ]))
        | (first_value ::_) ->
          let init =
            match m_init with
            | Some s -> s
            | None -> first_value
          in
          fun scope -> Let(name, Decl_Var(domain, init), scope)
      }

stmt:
  | SEMICOLON s
    { ParseTree($startpos, Nop) }
  | LBRACE s block=block RBRACE s
    { block }
  | cmd=cmd (**) args=expr* (**) terminator
    { ParseTree($startpos, Cmd(cmd, args)) }
  | name=name (**)
    LPAREN s
    args=separated_list(terminated(COMMA, s), terminated(expr, s))
    RPAREN terminator
    { ParseTree($startpos, CallMacro(name, args)) }
  | BIND s keys=expr s body=stmt
    { ParseTree($startpos, Bind(keys, body)) }
  | name=name (**) value=varinit terminator
    { ParseTree($startpos, Assign(name, value)) }
  | INCREMENT (**) name=name terminator
    { ParseTree($startpos, Increment(name)) }
  | c=condtype s name=name s LBRACE s cases=list(case) RBRACE s
    { ParseTree($startpos, Cond(c, name, cases)) }

varinit:
  | ASSIGN s value=str { value }

case:
  | value=pattern s RARROW s body=stmt { (value, body) }


(* These final productions appear inside other commands and don't consume any newlines *)

condtype:
  | WHEN   { Cond_When }
  | SWITCH { Cond_If }

pattern:
  | str      { Pat_Const $1 }
  | WILDCARD { Pat_Wildcard }

name:
  | name=IDENTIFIER
      { 
        if List.mem name.[0] ['+';'-'] then (
          raise (CompilationError([
            ($startpos, "Non-alias identifier starting with + or -")
          ]))
        );
        ($startpos, name)
      }

alias_name:
  | name=IDENTIFIER
      { ($startpos, name) }


cmd:  raw_cmd  { ($startpos, $1) }
expr: raw_expr { ($startpos, $1) }
str:  raw_str  { ($startpos, $1) }


raw_cmd:
  | raw_str { Left $1 }
  | QUOTED  { Right $1 }

raw_str:
  | IDENTIFIER { $1 }
  | UNQUOTED   { $1 }

raw_expr:
  | IDENTIFIER { $1 }
  | UNQUOTED   { $1 }
  | QUOTED     { $1 }
