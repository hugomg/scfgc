open Core_kernel.Std
open Types

module VarValue : sig
  type t = (VarId.t * int)
  include Comparable.S with type t := t
end = struct
  type t = (VarId.t * int)
  include Tuple.Comparable(VarId)(Int)
end


(* Convert conditionals to alias calls and variable assignments to alias mutation *)
let cprog_to_iprog cprog =
  
  let q_alias_types   = Queue.create () in
  let q_alias_defs    = Queue.create () in
  let q_toplevel_cmds = Queue.create () in


  let possible_assignments v = 
    let n = Map.find_exn cprog.c_var_domains v in
    List.map (List.range 0 n) ~f:(fun i -> (v,i))
  in

  let var_value_pairs =
    List.concat_map ~f:possible_assignments (Map.keys cprog.c_var_domains) in
  let var_set_aliases =
    var_value_pairs |>
    List.map ~f:(fun vi -> (vi, (AliasMode.None, AliasId.fresh ()))) |>
    VarValue.Map.of_alist_exn  in
  let var_set_cmds =
    var_value_pairs |>
    List.map ~f:(fun vi -> (vi, Queue.create())) |>
    VarValue.Map.of_alist_exn in


  let to_istmt = fold_cstmt ~f:(function
    | C_Do(x, xs) -> Istmt(I_Do(x, xs))
    | C_Call(aref) -> Istmt(I_Call(aref))
    | C_SetVar(varid, i) -> (
      let aref = Map.find_exn var_set_aliases (varid, i) in
      Istmt(I_Call(aref)) )

    | C_Cond(varid, cases) -> (
      let init = Map.find_exn cprog.c_var_initial_values varid in
      let case_aliases = Array.map cases ~f:(fun ss ->
            let aid = AliasId.fresh () in
            let aref = (AliasMode.None, aid) in
            Queue.enqueue q_alias_types (aid, ASimple);
            Queue.enqueue q_alias_defs (aref, ss);
            aref)
      in
      
      let aid = AliasId.fresh () in
      let aref = (AliasMode.None, aid) in
      Queue.enqueue q_alias_types (aid, ASimple);
      Queue.enqueue q_alias_defs (aref, [Istmt(I_Call(case_aliases.(init)))]);
      Array.iteri case_aliases ~f:(fun i caref ->
        let eventlist = Map.find_exn var_set_cmds (varid, i) in
        Queue.enqueue eventlist (Istmt(I_SetAlias(aref, [Istmt(I_Call(caref))])))
      );
      Istmt(I_Call(aref)) ))
  in


  Map.iter cprog.c_alias_types ~f:(fun ~key:aid ~data:typ ->
    Queue.enqueue q_alias_types (aid, typ));

  Map.iter cprog.c_alias_defs ~f:(fun ~key:aref ~data:ss ->
    Queue.enqueue q_alias_defs (aref, List.map ss ~f:to_istmt));

  List.iter cprog.c_toplevel ~f:(fun cstmt ->
    Queue.enqueue q_toplevel_cmds (to_istmt cstmt));
  
  Map.iter cprog.c_binds ~f:(fun ~key:k ~data:aref ->
    Queue.enqueue q_toplevel_cmds 
      (Istmt(I_Bind(k, [Istmt(I_Call(aref))]))) );

  List.iter cprog.c_events ~f:(fun (v, aref) ->
    List.iter (possible_assignments v) ~f:(fun vi ->
      let cmds = Map.find_exn var_set_cmds vi in
      Queue.enqueue cmds (Istmt(I_Call(aref)))));

  (* --- *)

  List.iter var_value_pairs ~f:(fun vi ->
      let (amode, aid) = Map.find_exn var_set_aliases vi in
      let cmds = Map.find_exn var_set_cmds vi in
      Queue.enqueue q_alias_types (aid, ASimple);
      Queue.enqueue q_alias_defs ((amode, aid), Queue.to_list cmds) );

  let toplevel_aliases = 
    Queue.to_list q_alias_defs |>
    List.map ~f:(fun (aref, ss) -> Istmt(I_SetAlias(aref, ss))) in

  check_iprog {
    i_alias_types = AliasId.Map.of_alist_exn @@ Queue.to_list q_alias_types;
    i_toplevel =
      toplevel_aliases @
      Queue.to_list q_toplevel_cmds ;
  }


(*** Inlining ***)

type count = {
  ct_assignments : int AliasRef.Table.t;
  ct_calls       : int AliasRef.Table.t;
}

let count_aliases iprog =
  let ct_calls       = AliasRef.Table.create () in
  let ct_assignments = AliasRef.Table.create () in
 
  Map.iter iprog.i_alias_types ~f:(fun ~key:aid ~data:typ ->
    match typ with
    | ASimple -> (
        Hashtbl.add_exn ct_calls ~key:(AliasMode.None, aid) ~data:0 )
    | APlusMinus -> (
        Hashtbl.add_exn ct_calls ~key:(AliasMode.Plus,  aid) ~data:0 ;
        Hashtbl.add_exn ct_calls ~key:(AliasMode.Minus, aid) ~data:0 )
    );
  
  List.iter iprog.i_toplevel
    ~f:(iter_istmt ~f:(function
      | I_Call aref         -> Hashtbl.incr ct_calls aref
      | I_SetAlias(aref, _) -> Hashtbl.incr ct_assignments aref
      | _ -> ()));

  { ct_assignments; ct_calls }


let arefs_with_count n tbl =
  AliasRef.Set.of_list @@ Hashtbl.keys @@ Hashtbl.filter tbl ~f:(fun x -> x = n)


(* If an alias is only defined once, we can lift its definition to the top 
 * of the file without changing the program. This is useful because it can turn things
 * like `alias foo "echo hello; alias bar baz"` into `alias foo echo hello`, which
 * doesn't need quotes *)
let lift_constant_aliases iprog =
  let {ct_assignments; _} = count_aliases iprog in
  let const_aliases = arefs_with_count 1 ct_assignments in

  let liftable_cmds = Queue.create () in
  List.iter iprog.i_toplevel
    ~f:(iter_istmt ~f:(fun istmt -> 
        match istmt with
        | I_SetAlias(aref, _) ->
          if Set.mem const_aliases aref then
            Queue.enqueue liftable_cmds (Istmt istmt)
        | _ -> ()
      ));

  let filtered_toplevel = List.filter_map iprog.i_toplevel
    ~f:(filter_istmt ~f:(function
      | I_SetAlias(aref, _)  when Set.mem const_aliases aref -> false
      | _ -> true
    ))
  in

  check_iprog {
    i_alias_types = iprog.i_alias_types;
    i_toplevel = Queue.to_list liftable_cmds @ filtered_toplevel;
  }

(* If a simple alias is constant and only called once, we can inline it *)
let inline_step iprog =
  let {ct_assignments; ct_calls} = count_aliases iprog in

  (* +- aliases are always from keybinds so we should not mess with them *)
  let is_simple = function 
    | (AliasMode.None, _) -> true
    | _ -> false
  in

  let const_aliases = arefs_with_count 1 ct_assignments in
  let removal_candidates =
    arefs_with_count 0 ct_calls |> Set.filter ~f:is_simple in
  let inline_candidates  =
    arefs_with_count 1 ct_calls |> Set.inter const_aliases |> Set.filter ~f:is_simple in

  let inline_cmds = AliasRef.Table.create () in
  List.iter iprog.i_toplevel
    ~f:(iter_istmt ~f:(function
        | I_SetAlias(aref, ss) ->
            if  Set.mem inline_candidates aref then
              Hashtbl.add_exn inline_cmds ~key:aref ~data:ss
        | _ -> ()
    ));


  let inlined_refs = AliasRef.Hash_set.create () in
  let rec inline is_bind istmt =
    let Istmt stmt = istmt in
    match stmt with
    | I_Bind(k, ss) ->
        let iss = match ss with
          | [] -> []
          | (s'::ss') ->
            let is'  = inline true s' in
            let iss' = List.map ~f:(inline false) ss' in
            List.concat (is' :: iss')
        in
        [ Istmt(I_Bind(k, iss)) ]
    | I_Call(aref) ->
        if Set.mem inline_candidates aref then
          let ss = Hashtbl.find_exn inline_cmds aref in
          match ss with
          | [] -> []
          | (Istmt(I_Call(AliasMode.Plus, _)) :: _) when is_bind ->
              [ istmt ]
          | (s'::ss')-> (
            Hash_set.strict_add_exn inlined_refs aref;
            let is' = inline is_bind s' in
            let iss' = List.map ~f:(inline false) ss' in
            List.concat (is' :: iss') )
        else
          [ istmt ]
    | I_SetAlias(aref, ss) ->
        if Set.mem inline_candidates aref then
          [ istmt ]
        else
          [ Istmt(I_SetAlias(aref, List.concat_map ~f:(inline false) ss)) ]
    | I_Do(_) ->
        [ istmt ]
  in

  let inlined_toplevel = List.concat_map iprog.i_toplevel ~f:(inline false) in

  let was_removed aref =
    (Set.mem removal_candidates aref) ||
    (Hash_set.mem inlined_refs aref)
  in

  check_iprog {
    i_alias_types = 
      Map.filter iprog.i_alias_types ~f:(fun ~key:aid ~data:_ ->
        not (was_removed (AliasMode.None, aid)) );
    i_toplevel =
      List.filter_map inlined_toplevel
        ~f:(filter_istmt ~f:(function
          | I_SetAlias(aref, _) when was_removed aref -> false
          | _ -> true
        ));
  }


let rec inline_constant_aliases iprog =
  let arefs1 = AliasId.Set.of_list (Map.keys iprog.i_alias_types) in
  let iprog' = inline_step iprog in
  let arefs2 = AliasId.Set.of_list (Map.keys iprog'.i_alias_types) in
  if AliasId.Set.equal arefs1 arefs2 then
    iprog'
  else
    inline_constant_aliases iprog'


(* Output *)

type rprog = 
  string list * (string * string list) list

(* Rename alias ids so they are contiguous *)
let compact_aliases iprog =
  let old_to_new_map =
    iprog.i_alias_types |>
    Map.keys |>
    List.dedup ~compare:AliasId.compare |>
    List.sort ~cmp: AliasId.compare |>
    List.mapi ~f:(fun i x -> (x,i)) |>
    AliasId.Map.of_alist_exn
  in
  (fun x -> Map.find_exn old_to_new_map x)

let has_quote s = String.contains s '"'

(* AFAIK, we only need to quote spaces, semicolons and double quotes but I'd
 * rather use a whitelist than a blacklist to be extra safe *)
let has_special_chars s = not @@
  String.for_all s ~f:(fun c ->
      Char.is_alphanum c || String.mem "./_+-" c )

let add_quotes s =
  assert (not (has_quote s)); (*see note [Quote-handling] *)
  "\"" ^ s ^ "\""

let rprog_of_iprog ~prefix iprog =

  let module FileId = Id.Make ( ) in

  assert (String.length prefix > 0);
  assert (String.for_all prefix ~f:(fun c -> c = '_' || Char.is_alphanum c));

  let exec_files = Queue.create () in

  let alias_to_int = compact_aliases iprog in
(*  let alias_to_int = AliasId.to_int in*)
  let serialize_alias (m, aid) = 
    let sm = match m with
      | AliasMode.None -> ""
      | AliasMode.Plus -> "+"
      | AliasMode.Minus -> "-" in
    sprintf "%s_aa_%s_%d" sm prefix (alias_to_int aid)
  in

  let serialize_file fid =
    let basename = sprintf "f%d.cfg" (FileId.to_int fid) in
    (basename, prefix^"/"^basename) (* dota is OK with forward-slash separators *)
  in

  let cmd_to_str (cmd, args) = 
    let argstrs = List.map args ~f:(fun arg ->
        if arg = "" || has_special_chars arg then
          add_quotes arg
        else
          arg
      ) in
    String.concat ~sep:" " (cmd :: argstrs)
  in

  let seq cmds =
    String.concat ~sep:";" (List.map ~f:cmd_to_str cmds)
  in

  let exec_spill cmds = 
    (* The source interpreter can't nest quotes and can't handle
     * individual commands with more than 499 characters (we stop at 300 to be safe) *)
    let cmdstrs = List.map cmds ~f:cmd_to_str in
    if List.exists cmdstrs ~f:has_quote || String.length (seq cmds) >= 300 then
      let (basename, relname) = serialize_file (FileId.fresh ()) in
      Queue.enqueue exec_files (basename, cmdstrs);
      [("exec",[relname])]
    else
      cmds
  in


  let convert = fold_istmt ~f:(function
      | I_Do(cmd, args) -> (cmd, args)
      | I_Call(aref) -> (serialize_alias aref, [])
      | I_SetAlias(aref, cmds) -> (
          (* See note [Quote handling]*)
          let no_semicolons (cmd, args) =
            List.for_all (cmd :: args) ~f:(fun x -> not (String.mem x ';'))
          in
          let said = serialize_alias aref in
          let spilt = match cmds with
            | [] -> cmds
            | [cmd] when no_semicolons cmd -> cmds
            | _ -> exec_spill cmds
          in
          match spilt with
          | []        -> ("alias", [said])
          | [(x,xs)]  -> ("alias", (said :: x :: xs))
          | (_::_::_) -> ("alias", [said; seq spilt])
        )
      | I_Bind(k, cmds) -> (
          match cmds with
          | [(x,[])] -> ("bind", [k; x])
          | _        -> ("bind", [k; seq @@ exec_spill cmds])
        )

    )
  in

  let main_cmds =
    List.map iprog.i_toplevel ~f:(fun stmt -> cmd_to_str (convert stmt)) in

  (main_cmds, Queue.to_list exec_files)


let print_to_stdout (main_cmds, helper_files) =
  let all_files = ("MAIN", main_cmds) :: helper_files in
  List.iter all_files ~f:(fun (filename, lines) ->
      printf "%s\n" filename;
      List.iter lines ~f:(fun line ->
          printf "  %s\n" line
        )
    )

(*  note [Quote handling] 
 *  =====================
 *
 *
 * Arguments for autoexec commands can be quoted with double quotes. However,
 * in the autoexec language there is no way to escape the quote character inside
 * a string, which means that in some situations we can't write nested alias or
 * bind commands directly. The workaround is to put the inner commands which
 * contain quotes in a separate file and invoke them with "exec that_file.cfg",
 * which doensn't require quotes itself.
 * 
 * That said, sending commands to a separate file at the first sight of nesting
 * makes the generated output very hard to read so we try to minimize that:
 * 
 * 
 * - We only have to quote commands with special characters (" " and ";")
 * 
 * - We don't need to quote spaces in the parameter to an alias command:
 * 
 *     alias xx echo foo   //this works
 *     alias xx "echo foo" //no need to use quotes like this
 *                         //which is what we need to do for "bind" cmds
 * 
 *   However, we still need to spill commands to a separate file if
 *   there are semicolons:
 * 
 *     alias xx alias yy "echo 1; echo 2"
 *     //This actually runs "echo 2" when xx is run
 *     //and yy is only bound to "echo 1"
 *     //which is not what you would expect from the quotes...
 *)

