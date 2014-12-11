open Core.Std

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


let map_pattern ~str = function
  | Pat_Wildcard -> Pat_Wildcard
  | Pat_Const s -> Pat_Const (str s)

let map_decl ~id ~expr ~str ~t = function
  | Decl_Alias(body) -> Decl_Alias(t body)
  | Decl_Var(domain, init) -> Decl_Var(List.map ~f:str domain, str init)
  | Decl_Const(value) -> Decl_Const(expr value)
  | Decl_Macro(argnames, body) -> Decl_Macro(List.map ~f:id argnames, t body)

let map_sstmtf ~id ~cmd ~expr ~str ~t = function
  | Nop -> Nop
  | Block(sts) -> Block(List.map ~f:t sts)
  | Let(name, decl, scope) -> Let(id name, map_decl ~id ~expr ~str ~t decl, t scope)
  | Cmd(x, xs) -> Cmd(cmd x, List.map ~f:expr xs)
  | CallMacro(name, args) -> CallMacro(id name, List.map ~f:expr args)
  | Bind(key, st) -> Bind(expr key, t st)
  | Assign(name, value) -> Assign(id name, str value)
  | Increment(name) -> Increment(id name)
  | Cond(ct, varname, cases) -> 
    Cond(ct, id varname, List.map cases ~f:(fun (pat, st) -> (map_pattern ~str pat, t st)))

type pos = Lexing.position

let compare_pos p1 p2 =
  let open Lexing in
  let r = Int.compare p1.pos_lnum p2.pos_lnum in
  if r <> 0 then r else
  let r = Int.compare p1.pos_cnum p2.pos_cnum in
  if r <> 0 then r else
  0

let pos_fname p = p.Lexing.pos_fname
let pos_lnum  p = p.Lexing.pos_lnum
let pos_cnum  p = p.Lexing.pos_cnum - p.Lexing.pos_bol

exception CompilationError of (pos * string) list

let with_errors body =
  let error_queue = Queue.create () in
  let r = body (fun p msg -> Queue.enqueue error_queue (p,msg)) in
  if Queue.is_empty error_queue then
    r
  else
    let errors =
      List.sort
        ~cmp:(fun (p1, _) (p2,_) -> compare_pos p1 p2)
        (Queue.to_list error_queue) in
    raise (CompilationError errors)


type parse_tree = 
  ParseTree of
    pos *
    ( pos * string,
      pos * (string, string) Either.t,
      pos * string,
      pos * string,
      parse_tree
    ) sstmtf


let rec sexp_of_parse_tree (ParseTree(_, st)) =
  sexp_of_sstmtf
    (fun (_, s) -> String.sexp_of_t s)
    (fun (_, e) -> Either.sexp_of_t String.sexp_of_t String.sexp_of_t e)
    (fun (_, s) -> String.sexp_of_t s)
    (fun (_, s) -> String.sexp_of_t s)
    sexp_of_parse_tree
    st


module AliasId = Id.Make( )
module VarId = Id.Make( )

type alias_type = ASimple | APlusMinus with sexp

module AliasMode = struct
  module T = struct 
    type t =  None | Plus | Minus with sexp
    let to_int = function
      | None -> 0
      | Plus -> 1
      | Minus -> 2
    let compare a1 a2 = Int.compare (to_int a1) (to_int a2)
    let hash x = Int.hash (to_int x)
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)
end

module AliasRef = struct
  type t = (AliasMode.t * AliasId.t) with sexp
  include Tuple.Comparable(AliasMode)(AliasId)
  include Tuple.Hashable(AliasMode)(AliasId)
end

(* With conditionals *)

type 't cstmtf =
  | C_SetVar of VarId.t * int
  | C_Cond of VarId.t * 't list array
  | C_Do of string * string list
  | C_Call of AliasRef.t
with sexp

type cstmt =
    Cstmt of cstmt cstmtf
with sexp

type cprog = {
  c_var_initial_values : int VarId.Map.t;
  c_var_domains: int VarId.Map.t;
  c_alias_types: alias_type AliasId.Map.t;
  c_alias_defs: cstmt list AliasRef.Map.t;
  c_binds: AliasRef.t String.Map.t;
  c_events: (VarId.t * AliasRef.t) list;
  c_toplevel: cstmt list
} with sexp

let map_cstmtf ~f = function
  | C_SetVar(v, n) -> C_SetVar(v, n)
  | C_Cond(v, cases) -> C_Cond(v, Array.map cases ~f:(List.map ~f))
  | C_Do(x, xs) -> C_Do(x, xs)
  | C_Call(aref) -> C_Call(aref)

let rec fold_cstmt ~f (Cstmt stmt) = 
  f (map_cstmtf stmt ~f:(fold_cstmt ~f))
  
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

let map_istmtf ~f = function
  | I_Do(cmd, args) -> I_Do(cmd, args)
  | I_Call(aref) -> I_Call(aref)
  | I_SetAlias(aref, sts) -> I_SetAlias(aref, List.map ~f sts)
  | I_Bind(key, sts) -> I_Bind(key, List.map ~f sts)

let rec fold_istmt ~f (Istmt stmt) = 
  f (map_istmtf stmt ~f:(fold_istmt ~f))

let rec iter_istmt ~f (Istmt stmt) =
  f stmt;
  ignore @@ map_istmtf stmt ~f:(iter_istmt ~f)

let rec filter_istmt ~f (Istmt stmt) = 
  let ff = function
    | Some (Istmt s) when f s -> Some (Istmt s)
    | _ -> None
  in
  let stmt' = match map_istmtf stmt ~f:(filter_istmt ~f) with
    | I_Bind(key, ss) -> I_Bind(key, List.filter_map ~f:ff ss)
    | I_SetAlias(aref, ss) -> I_SetAlias(aref, List.filter_map ~f:ff ss)
    | I_Call(aref) -> I_Call(aref)
    | I_Do(cmd, args) -> I_Do(cmd, args)
  in
  if f stmt' then Some (Istmt stmt') else None


(* Sanity check invariants *)

let check_aref alias_types (amode, aid) =
  match Map.find_exn alias_types aid with
  | ASimple    -> assert (amode = AliasMode.None)
  | APlusMinus -> assert (amode = AliasMode.Plus || amode = AliasMode.Minus)

let check_cstmtf var_domains alias_types = function
  | C_SetVar(v, i) -> (
      let d = Map.find_exn var_domains v in
      assert (0 <= i && i < d) )
  | C_Cond(v, bs) -> (
      let d = Map.find_exn var_domains v in
      assert (d = Array.length bs) )
  | C_Call(aref) -> (
      check_aref alias_types aref)
  | C_Do _ -> ()


let check_cprog cprog =

  let checkaref aref =
    check_aref cprog.c_alias_types aref
  in
  let rec checkstmt (Cstmt stmt) =
    check_cstmtf cprog.c_var_domains cprog.c_alias_types stmt;
    ignore @@ map_cstmtf stmt ~f:checkstmt
  in
  Map.iter cprog.c_alias_defs ~f:(fun ~key:aref ~data:stmts ->
    checkaref aref;
    List.iter stmts ~f:checkstmt);
  Map.iter cprog.c_binds ~f:(fun ~key:_ ~data:aref ->
    checkaref aref );
  List.iter cprog.c_events ~f:(fun (v, aref) ->
      assert (Map.mem cprog.c_var_domains v);
      checkaref aref );
  List.iter cprog.c_toplevel ~f:checkstmt;

  cprog

let check_istmtf alias_types = function 
  | I_Call(aref) -> check_aref alias_types aref
  | I_SetAlias(aref, _) -> check_aref alias_types aref
  | _ -> ()

let check_iprog iprog =
  let rec check (Istmt stmt) =
    check_istmtf iprog.i_alias_types stmt;
    ignore @@ map_istmtf stmt ~f:check
  in
  List.iter iprog.i_toplevel ~f:check;
  iprog

