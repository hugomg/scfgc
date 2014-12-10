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
(* EOF *)
%token EOF

%start prog

%type <Types.parse_tree> prog
%%

prog: 
  | block EOF { $1 }

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
  | DEFINE name=name value=expr SEMICOLON
      { fun scope -> Let(name, Decl_Const(value), scope) }
  | DEFINE name=name LPAREN args=separated_list(COMMA, name) RPAREN body=stmt
      { fun scope -> Let(name, Decl_Macro(args, body), scope) }
  | ALIAS name=alias_name body=stmt
      { fun scope -> Let(name, Decl_Alias(body), scope) }
  | VAR name=name 
    COLON LBRACE domain=separated_list(COMMA, str) RBRACE m_init=ioption(varinit)
    SEMICOLON
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
  | SEMICOLON
    { ParseTree($startpos, Nop) }
  | LBRACE block=block RBRACE
    { block }
  | cmd=cmd args=expr* SEMICOLON
    { ParseTree($startpos, Cmd(cmd, args)) }
  | name=name LPAREN args=separated_list(COMMA, expr) RPAREN SEMICOLON
    { ParseTree($startpos, CallMacro(name, args)) }
  | BIND keys=expr body=stmt
    { ParseTree($startpos, Bind(keys, body)) }
  | name=name ASSIGN value=str SEMICOLON
    { ParseTree($startpos, Assign(name, value)) }
  | INCREMENT name=name SEMICOLON
    { ParseTree($startpos, Increment(name)) }
  | c=condtype name=name LBRACE cases=list(case) RBRACE
    { ParseTree($startpos, Cond(c, name, cases)) }

varinit:
  | ASSIGN value=str { value }

condtype:
  | WHEN   { Cond_When }
  | SWITCH { Cond_If }

case:
  | value=pattern RARROW body=stmt { (value, body) }

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
