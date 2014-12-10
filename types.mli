open Core.Std

(* The syntax trees for the parser and the typechecker have a similar
 * structure so we define them using a common functor that is heaviliy parameterized:
 *
 * t   -> "recursion" parameter, to delay tying the knot
 * id  -> identifiers
 * cmd -> command names
 * expr -> command arguments (interpolateable string)
 * str -> enum names
 * *)


type condtype =
  | Cond_If
  | Cond_When
with sexp

type 'str pattern =
  | Pat_Wildcard
  | Pat_Const of 'str
with sexp

type ('id, 'expr, 'str, 't) decl =
  | Decl_Alias of 't
  | Decl_Var   of 'str list * 'str
  | Decl_Const of 'expr
  | Decl_Macro of 'id list * 't
with sexp

type ('id, 'cmd, 'expr, 'str, 't) sstmtf =
  (* Structuring *)
  | Nop
  | Block of 't list
  (* Definitions *)
  | Let of 'id * ('id, 'expr, 'str, 't) decl * 't
  (* Instructions *)
  | Cmd of 'cmd * 'expr list
  | CallMacro of 'id * 'expr list
  | Bind of 'expr * 't
  | Assign of 'id * 'str
  | Increment of 'id
  | Cond of condtype * 'id * ('str pattern * 't) list
with sexp

val map_pattern:
  str: ('s1 -> 's2) ->
  's1 pattern ->
  's2 pattern

val map_decl:
  id:   ('i1 -> 'i2) ->
  expr: ('e1 -> 'e2) ->
  str:  ('s1 -> 's2) ->
  t:    ('t1 -> 't2) ->
  ('i1, 'e1, 's1, 't1) decl ->
  ('i2, 'e2, 's2, 't2) decl

val map_sstmtf:
  id:   ('i1 -> 'i2) ->
  cmd:  ('c1 -> 'c2) ->
  expr: ('e1 -> 'e2) ->
  str:  ('s1 -> 's2) ->
  t:    ('t1 -> 't2) ->
  ('i1, 'c1, 'e1, 's1, 't1) sstmtf ->
  ('i2, 'c2, 'e2, 's2, 't2) sstmtf


(* Source code positions
 * ===================== *)

type pos = Lexing.position

val pos_to_string: pos -> string

(* Helper function so we can signal an error and keep processing 
 * to find more errors instead of immediately throwing an exception *)
exception CompilationError of (pos * string) list
val with_errors: ((pos -> string -> unit) -> 'a) -> 'a 


(* Concrete syntax tree
 * ==================== *)

type parse_tree =
    ParseTree of
      pos *
      ( pos * string,
        pos * (string, string) Either.t,
        pos * string,
        pos * string,
        parse_tree
      ) sstmtf

val sexp_of_parse_tree : parse_tree -> Sexp.t

(* Intermediate representation
 * ===========================
 *  - with mutable aliases *)

module AliasId: Id.S
module VarId: Id.S

type alias_type = ASimple | APlusMinus

module AliasMode : sig
  type t = None | Plus | Minus with sexp
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module AliasRef : sig
  type t = (AliasMode.t * AliasId.t) with sexp
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

(* Without conditionals *)

type 't istmtf =
  | I_Do of string * string list
  | I_Call of AliasRef.t
  | I_SetAlias of AliasRef.t * 't list
  | I_Bind of string * 't list
with sexp

type istmt = 
    Istmt of istmt istmtf
with sexp

type iprog = {
  i_alias_types: alias_type AliasId.Map.t;
  i_toplevel: istmt list;
}

val map_istmtf:
  f:('a -> 'b) ->
  'a istmtf -> 'b istmtf

val fold_istmt:
  f:('r istmtf -> 'r) ->
  istmt -> 'r

val iter_istmt:
  f:(istmt istmtf -> unit) ->
  istmt -> unit

val filter_istmt:
  f:(istmt istmtf -> bool) ->
  istmt -> istmt option

(* With conditionals *)

type 't cstmtf =
  | C_I of 't istmtf
  | C_SetVar of VarId.t * int
  | C_Cond of VarId.t * 't list array
with sexp

type cstmt =
    Cstmt of cstmt cstmtf
with sexp

type cprog = {
  c_var_initial_values : int VarId.Map.t;
  c_var_domains: int VarId.Map.t;
  c_alias_types: alias_type AliasId.Map.t;
  c_toplevel: cstmt list
}

val map_cstmtf:
  f:('a -> 'b) ->
  'a cstmtf -> 'b cstmtf

val fold_cstmt:
  f:('r cstmtf -> 'r) ->
  cstmt -> 'r

val check_iprog: iprog -> iprog
val check_cprog: cprog -> cprog
