open Core.Std
open Types

(* ----------- *)

module NameId = Id.Make( )

type expr =
  | Interpolation of NameId.t list * (string list -> string)

type cmd =
  | Cmd_Const of string
  | Cmd_Dyn of expr

type symbol_table =
  (string * NameId.t) list

type bound_tree =
    BoundTree of
      pos *
      symbol_table *
      ( pos * NameId.t,
        pos * cmd,
        pos * expr,
        pos * string,
        bound_tree
      ) sstmtf


type bound_decl =
  (NameId.t, expr, string, bound_tree) decl

type name_type =
  | T_Const
  | T_Func of int
  | T_Var of string list * string
  | T_Alias


(* --------- *)


let interpolate_re =
  let open Re2.Std in
  Re2.create_exn "\\$([A-Za-z_0-9]*)"


let bind_names parse_tree = with_errors (fun error ->

  let decltable = NameId.Table.create () in

  let noone = NameId.fresh() in (* to keep chugging along after finding an error *)
  
  let rec bind symbol_table (ParseTree(pos, stmt)) =

    let lookup_name pos name =
      match List.Assoc.find symbol_table name with
      | Some nid -> nid
      | None -> error pos (sprintf "Undefined name %s" name); noone
    in

    let interpolate_string pos rawstr =
      let open Re2.Std in
      let ms = Re2.get_matches_exn interpolate_re rawstr in
      let names =
        List.dedup ~compare:String.compare @@
        List.map ms ~f:(fun m -> Re2.Match.get_exn ~sub:(`Index 1) m) in
      let nids  = List.map names ~f:(lookup_name pos) in
      Interpolation(nids, fun values ->
          let name_to_value = List.zip_exn names values in
          Re2.replace_exn interpolate_re rawstr ~f:(fun m ->
              let name = Re2.Match.get_exn ~sub:(`Index 1) m in
              List.Assoc.find_exn name_to_value name
            ))
    in
    let do_id (pos, name) =
      (pos, lookup_name pos name)
    in

    let do_expr (pos, name) =
      (pos, interpolate_string pos name)
    in

    let do_cmd (pos, name) =
      (* Commands with no quotes and nt interpolations
       * are potentially an alias call *)
      let cmd =
        match name with
        | Either.Right s -> Cmd_Dyn (interpolate_string pos s)
        | Either.Left s ->
          let ip = interpolate_string pos s in
          match ip with
          | Interpolation([], _) -> Cmd_Const s
          | _ -> Cmd_Dyn ip
      in
      (pos, cmd)
    in
    let do_str = ident in

    let stmt' = match stmt with
      | Let((npos, name), decl, scope) ->
        let nid = NameId.fresh () in
        let symbol_table' = (name, nid)::symbol_table in
        let decl' =
          match decl with
          | Decl_Macro(params, body) ->
            let nameids = List.map params ~f:(fun pname -> (pname, NameId.fresh())) in
            let pnids       = List.map nameids ~f:(fun ((pos, _), nameid) -> (pos, nameid)) in
            let param_names = List.map nameids ~f:(fun ((_, name), nameid) -> (name, nameid)) in
            let symbol_table'' = List.rev_append param_names symbol_table' in
            Decl_Macro(pnids, bind symbol_table'' body)
          | _ ->
            map_decl decl ~id:do_id ~expr:do_expr ~str:do_str ~t:(bind symbol_table')
        in
        Hashtbl.add_exn
          decltable
          ~key:nid
          ~data:(map_decl decl' ~id:snd ~expr:snd ~str:snd ~t:ident);
        Let((npos, nid), decl', (bind symbol_table' scope))

      | _ ->
        map_sstmtf stmt ~id:do_id ~cmd:do_cmd ~expr:do_expr ~str:do_str  ~t:(bind symbol_table)

    in
    BoundTree(pos, symbol_table, stmt')
  in

  let tree = bind [] parse_tree in
  let decls = NameId.Map.of_alist_exn @@ Hashtbl.to_alist @@ decltable in
  (decls, tree)
)

let typecheck bound_tree = with_errors (fun error ->

  let types = NameId.Table.create () in

  let assert_is_const (pos,nid) = 
    match Hashtbl.find_exn types nid with
    | T_Const -> Some ()
    | _ -> error pos "Not a constant or function parameter"; None
  in

  let assert_is_function (pos,nid) =
    match Hashtbl.find_exn types nid with
    | T_Func(expected) -> Some expected
    | _ -> error pos "Not a function"; None
  in

  let assert_is_variable (posvar,nid) =
    match Hashtbl.find_exn types nid with
    | T_Var(domain, _) -> Some domain
    | _ -> error posvar "Not a variable"; None
  in


  let assert_function_params (pos, nid) passed =
    match assert_is_function (pos, nid) with
    | None -> None
    | Some expected ->
      if passed = expected then Some ()
      else (error pos (sprintf "Passed %d parameters, expected %d" passed expected); None)
  in
  
  let assert_variable_value pvar (posvalue, value) =
    match assert_is_variable pvar with
    | None -> None
    | Some domain ->
      if List.mem domain value then Some ()
      else (error posvalue (sprintf "Value %S is not in the variable's domain" value); None)
  in


  let rec check (BoundTree(startpos, _, stmt)) =

    (match stmt with
    | Let((pos,nid), decl, _) -> (
      match decl with
       | Decl_Const(_) ->
         Hashtbl.add_exn types ~key:nid ~data:T_Const
       | Decl_Macro(params, _) -> (
           Hashtbl.add_exn types ~key:nid ~data:(T_Func (List.length params));
           List.iter params ~f:(fun (_,pnid) ->
               Hashtbl.add_exn types ~key:pnid ~data:T_Const))
       | Decl_Alias(_) ->
         Hashtbl.add_exn types ~key:nid ~data:T_Alias
       | Decl_Var(pdomain, (posinit, init)) -> (
           let domain = List.map pdomain ~f:snd in
           Hashtbl.add_exn types ~key:nid ~data:(T_Var(domain, init));
           ignore @@ assert_variable_value (pos,nid) (posinit, init))
      )

    | CallMacro(pfunc, pparams) ->
        ignore @@ assert_function_params pfunc (List.length pparams)

    | Assign(pvar, pvalue) ->
        ignore @@ assert_variable_value pvar pvalue

    | Increment(pvar) ->
        ignore @@ assert_is_variable pvar

    | Cond(_, pvar, cases) -> (
        match (assert_is_variable pvar) with
        | None -> ()
        | Some domain -> 
            match Option.all (List.map cases ~f:(fun(pat, _body) ->
              match pat with
              | Pat_Wildcard -> Some ()
              | Pat_Const pvalue -> assert_variable_value pvar pvalue))
            with
            | None -> ()
            | Some _ ->
                List.iter domain ~f:(fun value ->
                  if
                    List.exists cases ~f:(fun(pat, _body) ->
                      match pat with
                      | Pat_Wildcard -> true
                      | Pat_Const (_,patvalue) when (value=patvalue) -> true
                      | _ -> false)
                  then
                    ()
                  else
                    error startpos (sprintf "Non-exaustive pattern. Missing case %S" value)
                )
      )

    | _ -> ()
    );


    let check_expr (pos, expr) =
      let Interpolation(nids, _) = expr in
      List.iter nids ~f:(fun nid ->
          ignore @@ assert_is_const (pos, nid)
        )
    in

    let check_cmd (pos, bcmd) = 
      match bcmd with
      | Cmd_Const _ -> ()
      | Cmd_Dyn expr -> check_expr (pos, expr)
    in

    ignore @@ map_sstmtf stmt ~id:ident ~cmd:check_cmd ~expr:check_expr ~str:ident ~t:check
  in

  check bound_tree;
  NameId.Map.of_alist_exn @@ Hashtbl.to_alist types

)




type cmd_bind =
  | CmdB_Primitive
  | CmdB_Alias of NameId.t

type 'stmt keybind = {
  b_pos : pos;
  b_modifiers : string list;
  b_key : string;
  b_onkeydown : 'stmt list;
  b_onkeyup : 'stmt list;
}

let reverse_map_exn ~comparator m =
  m |>
  Map.to_alist |>
  List.map ~f:(fun (a,b) -> (b,a)) |>
  Map.of_alist_exn ~comparator

let merge_map_exn m1 m2 =
  Map.merge m1 m2 ~f:(fun ~key:_ v ->
    match v with
    | `Both(_,_) -> assert false
    | `Left x  -> Some x
    | `Right x -> Some x)

let plus_alias_re =
  let open Re2.Std in
  Re2.create_exn "^\\+"

let eval_bound_program
  (decls : bound_decl NameId.Map.t)
  (types : name_type NameId.Map.t)
  (bound_tree : bound_tree)
  error
=

  let keybind_modifiers = String.Hash_set.create () in
  let keybinds = Queue.create () in

  let name_to_alias_id = 
    Map.filter_map types ~f:(fun t ->
      match t with T_Alias -> Some(AliasId.fresh ()) | _ -> None)
  in

  let alias_id_to_name =
    reverse_map_exn name_to_alias_id ~comparator:AliasId.comparator in

  let name_to_var_id = 
    Map.filter_map types ~f:(fun t ->
      match t with T_Var _ -> Some(VarId.fresh ()) | _ -> None)
  in

  let var_id_to_name = 
    reverse_map_exn name_to_var_id ~comparator:VarId.comparator in

  let get_domain nid =
    match Map.find_exn decls nid with
    | Decl_Var(domain_strings, init) ->
        let domain = List.mapi domain_strings ~f:(fun i s -> (s, i)) in
        let iinit = List.Assoc.find_exn domain init ~equal:String.equal in
        (domain, iinit)
    | _ -> assert false (* type error *)
  in

  let rec eval (is_top:bool) param_values (BoundTree(startpos, symbol_table, stmt)) =
    
    let interpolate (Interpolation(nids, func)) =
      func (List.map nids ~f:(List.Assoc.find_exn ~equal:NameId.equal param_values))
    in

    let do_expr (_, ip) =
      interpolate ip
    in

    let do_cmd (_, cmd) =
      match cmd with
      | Cmd_Dyn expr ->
          (CmdB_Primitive, interpolate expr)
      | Cmd_Const name ->
          let typ =
            match (List.Assoc.find symbol_table name) with
            | None -> CmdB_Primitive
            | Some nid ->
                match (Map.find_exn types nid) with
                | T_Alias -> CmdB_Alias nid
                | _ -> CmdB_Primitive
          in
          (typ, name)
    in
    
    let stmt_with_children_converted
      : (NameId.t, cmd_bind * string,  string, string, cstmt list) sstmtf
    =
      match stmt with
      | Let (pnid, decl, scope) -> (
        let (_, nid) = pnid in
        let param_values' =
          match decl with
          | Decl_Const expr -> (nid, do_expr expr) :: param_values
          | _ -> param_values
        in
        let scope' = eval is_top param_values' scope in
        let rec_decl = 
          match decl with
          | Decl_Alias(_) -> eval false param_values 
          | _ -> (fun _ -> [])
        in
        let decl' =
          map_decl decl
            ~id:snd
            ~expr:do_expr
            ~str:snd
            ~t:rec_decl
        in
        Let(nid, decl', scope') )
      | Bind(_) ->
        map_sstmtf stmt
          ~id:snd
          ~cmd:do_cmd
          ~expr:do_expr
          ~str:snd
          ~t:(eval false param_values)
      | _ ->
        map_sstmtf stmt
          ~id:snd
          ~cmd:do_cmd
          ~expr:do_expr
          ~str:snd
          ~t:(eval is_top param_values)
    in

    match stmt_with_children_converted with
    | Nop -> []
    | Block stmts -> List.concat stmts
    | Cmd((cmdtyp, cmd), args) -> (
        match cmdtyp with
        | CmdB_Primitive ->
            [ Cstmt(C_I(I_Do(cmd, args))) ]
        | CmdB_Alias anid ->
            if not (List.is_empty args) then error startpos "Calling an alias with parameters";
            let aid = Map.find_exn name_to_alias_id anid in
            [ Cstmt(C_I(I_Call(AliasMode.None, aid))) ])
    | Let(nid, decl, scope) -> (
        let stmt'=
          match decl with
          | Decl_Alias(body) ->
              let aid = Map.find_exn name_to_alias_id nid in
              [ Cstmt(C_I(I_SetAlias((AliasMode.None, aid), body))) ]
          | _ -> []
        in
        stmt' @ scope )
    | CallMacro(nid, params) -> (
        let param_nids, body =
          match (Map.find_exn decls nid) with
          | Decl_Macro(param_nids, body) -> (param_nids, body)
          | _ -> assert false
        in
        let params' = List.zip_exn param_nids params in
        eval is_top (List.rev_append params' param_values) body )
    | Assign(nid, value) -> (
        let var_id = Map.find_exn name_to_var_id nid in
        let domain, _  = get_domain nid in
        let ivalue = List.Assoc.find_exn domain value ~equal:String.equal in
        [ Cstmt(C_SetVar(var_id, ivalue)) ] )
    | Increment(nid) -> (
        let var_id = Map.find_exn name_to_var_id nid in
        let domain, _ = get_domain nid in
        let n = List.length domain in
        let cases = List.map domain ~f:(fun (_, i) -> [Cstmt(C_SetVar(var_id, (i+1)%n))]) in
        [ Cstmt(C_Cond(var_id, Array.of_list cases)) ] )
    | Cond(ct, nid, cases) -> (
        if is_top && ct = Cond_If then
          error startpos "If statement at toplevel (use when instead)"
        ;
        if not is_top && ct = Cond_When then
          error startpos "When statements cannot be inside alias or bind callbacks"
        ;
        let var_id = Map.find_exn name_to_var_id nid in
        let domain, _ = get_domain nid in
        let ifcases = List.map domain ~f:(fun (value, _) ->
          match
            List.find_map cases ~f:(fun (pat, branch_stmts) ->
              match pat with
              | Pat_Wildcard ->
                  Some branch_stmts
              | Pat_Const patvalue when patvalue = value ->
                  Some branch_stmts
              | _ ->
                  None )
          with
          | None -> assert false (* non-exaustive pattern match *)
          | Some sts -> sts
        ) in
        [ Cstmt(C_Cond(var_id, Array.of_list ifcases)) ] )
    | Bind(keys, body) -> (
        if not is_top then
          error startpos "Must not call bind from inside an alias or bind callback"
        ;

        let onkeydown, onkeyup = 
          match body with
          | [] -> ([], [])
          | (plus_cmd :: other_cmds) -> (
            (* If the first command in a bind starts with a "+",
             * it implicitly registers the "-" version of the
             * command to be run when the key is released *)
            let mplus_cmdname = 
              match plus_cmd with
              | Cstmt(C_I(I_Do(cmdname, _))) ->
                Some cmdname
              | Cstmt(C_I(I_Call(amode, aid))) ->
                assert (amode = AliasMode.None);
                let plus_nid = Map.find_exn alias_id_to_name aid in
                Some (fst @@ List.find_exn symbol_table 
                        ~f:(fun (_name, nid) -> plus_nid = nid))
              | _ ->
                None
            in

            let onkeydown =
              match mplus_cmdname with
              | None -> []
              | Some plus_cmdname -> (
                  let open Re2.Std in
                  let minus_cmdname =
                    Re2.replace_exn
                      plus_alias_re
                      plus_cmdname
                      ~f:(fun _ -> "-") in
                  if plus_cmdname = minus_cmdname then
                    []
                  else
                    let minus_cmd =
                      match plus_cmd with
                      | Cstmt(C_I(I_Do(_, args))) ->
                          Cstmt(C_I(I_Do(minus_cmdname, args)))
                      | Cstmt(C_I(I_Call(amode, _))) -> (
                          assert (amode = AliasMode.None);
                          match
                            List.Assoc.find symbol_table minus_cmdname
                              ~equal:String.equal
                          with
                          | None -> error startpos (sprintf "Alias %s not in scope" minus_cmdname); plus_cmd
                          | Some minus_nid -> 
                            let minus_aid = Map.find_exn name_to_alias_id minus_nid in
                            Cstmt(C_I(I_Call(amode, minus_aid))) )
                      | _ ->
                          assert false
                    in
                    (minus_cmd :: other_cmds) )
            in
            (body, onkeydown) )
        in

        let ks =
          String.split (String.uppercase keys) ~on:' ' |>
          List.filter ~f:(fun s -> not (String.is_empty s)) in

        (match List.rev ks with
        | [] -> error startpos "Empty keybind declaration"
        | (key::mods) -> (
            List.iter mods ~f:(Hash_set.add keybind_modifiers);
            if Hash_set.mem keybind_modifiers key then
              error startpos (sprintf "Cannot use %s as a keybind - its already used as a modifier" key)
            else
              Queue.enqueue keybinds {
                b_pos = startpos;
                b_modifiers = mods;
                b_key = key;
                b_onkeyup = onkeyup;
                b_onkeydown = onkeydown;
              }
        ));
        [] (*keybinds aren't added to the toplevel just yet*) )
  in


  
  let toplevel = eval true [] bound_tree in
  
  let var_initial_values = Map.map var_id_to_name ~f:(fun vname ->
    let _, init = get_domain vname in init ) in

  let var_domains = Map.map var_id_to_name ~f:(fun vname ->
    let domain, _ = get_domain vname in  List.length domain ) in

  let alias_types = Map.map alias_id_to_name ~f:(fun _ -> ASimple) in

  let prog_without_binds = {
    c_var_initial_values = var_initial_values;
    c_var_domains = var_domains;
    c_alias_types = alias_types;
    c_toplevel = toplevel;
  } in

  (prog_without_binds, Hash_set.to_list keybind_modifiers, Queue.to_list keybinds)

let convert_keybinds
  (mod_keys : string list)
  (keybinds : cstmt keybind list)
  error
  =

  let modifier_variables =
    mod_keys |>
    List.map  ~f:(fun m -> (m, VarId.fresh ())) |>
    String.Map.of_alist_exn in

  let r_modifier_variables =
    reverse_map_exn modifier_variables ~comparator:VarId.comparator in
  let modifier_var_domains =
    Map.map r_modifier_variables ~f:(fun _ -> 2) in
  let modifier_var_initial_values =
    Map.map r_modifier_variables ~f:(fun _ -> 0) in

  let modifier_aliases = 
    mod_keys |>
    List.map  ~f:(fun m -> (m, AliasId.fresh ())) |>
    String.Map.of_alist_exn in
  
  let r_modifier_aliases =
    reverse_map_exn modifier_aliases ~comparator:AliasId.comparator in
  let modifier_alias_types = 
    Map.map r_modifier_aliases ~f:(fun _ -> APlusMinus) in
 
  let modifier_toplevel = List.concat_map mod_keys ~f:(fun m ->
    let varid = Map.find_exn modifier_variables m in
    let alias_id = Map.find_exn modifier_aliases m in
    [
      Cstmt(C_I(I_Bind(m, [Cstmt(C_I(I_Call(AliasMode.Plus, alias_id)))])));
      Cstmt(C_I(I_SetAlias((AliasMode.Plus,  alias_id), [ Cstmt(C_SetVar(varid, 1)) ])));
      Cstmt(C_I(I_SetAlias((AliasMode.Minus, alias_id), [ Cstmt(C_SetVar(varid, 0)) ])));
    ] )
  in

  let keybinds_by_key = 
    keybinds |>
    List.map ~f:(fun b -> (b.b_key, b)) |>
    String.Map.of_alist_fold ~init:[] ~f:(fun bs b -> b :: bs) |>
    Map.to_alist in

  (* Group keybinds by what modifier keys they use *)
  let rec casify binds used_mods unsorted_mods cb =
    match unsorted_mods with
    | [] -> (
      match binds with
        | [] -> []
        | [{b_onkeydown; b_onkeyup;_}] ->
            cb b_onkeydown b_onkeyup
        | bs -> 
          List.iter bs ~f:(fun b ->
            let keycombo = String.concat ~sep:"+" (used_mods @ [b.b_key]) in
            error b.b_pos (sprintf "Conflicting keybinds for %s" keycombo) );
          []
        )
    | (m :: ms) ->
      let (pressed, unpressed) =
        List.partition_tf binds ~f:(fun {b_modifiers;_} ->
          List.mem ~equal:String.equal b_modifiers m )
      in
      let v = Map.find_exn modifier_variables m in
      [Cstmt(C_Cond(v, [|
           casify unpressed     used_mods  ms cb;
           casify pressed   (m::used_mods) ms cb;
         |]))]
  in

  let keybind_alias_types = Queue.create () in
  let add_fresh_alias typ =
    let aid = AliasId.fresh () in
    Queue.enqueue keybind_alias_types (aid, typ);
    aid
  in

  let keybind_toplevel = Queue.create () in

  List.iter keybinds_by_key ~f:(fun (k, binds) ->
      if List.for_all binds ~f:(fun b -> List.is_empty b.b_onkeyup) then
        let cmds = casify binds [] mod_keys (fun onkeydown _onkeyup ->
            let cb_aid = add_fresh_alias ASimple in
            Queue.enqueue keybind_toplevel (Cstmt(C_I(I_SetAlias((AliasMode.None, cb_aid), onkeydown))));
            [ Cstmt(C_I(I_Call(AliasMode.None, cb_aid))) ]
          ) in
        Queue.enqueue keybind_toplevel @@
          (Cstmt(C_I(I_Bind(k, cmds))))

      else
        (* If we have any statements that should be run when a key is released then we must
         * make sure that we only ever call "bind" once, using indirection through an alias for
         * the multiple cases. If we call "bind" again in the middle of a "bind k +foo" call, the
         * system forgets the old bind and doesn't call the "-foo" on release. This can happen when
         * you release one of the modifier keys before you release the main key *)
        let bind_aid = add_fresh_alias APlusMinus in
        let cmds = casify binds [] mod_keys (fun onkeydown onkeyup ->
            let cb1_aid = add_fresh_alias ASimple in
            let cb2_aid = add_fresh_alias ASimple in
            Queue.enqueue keybind_toplevel (Cstmt(C_I(I_SetAlias((AliasMode.None, cb1_aid), onkeydown))));
            Queue.enqueue keybind_toplevel (Cstmt(C_I(I_SetAlias((AliasMode.None, cb2_aid), onkeyup))));
            [
              Cstmt(C_I(I_Call(AliasMode.None, cb1_aid)));
              Cstmt(C_I(I_SetAlias((AliasMode.Minus, bind_aid), [Cstmt(C_I(I_Call(AliasMode.None, cb2_aid)))])));
            ]
          ) in

        Queue.enqueue keybind_toplevel @@
          Cstmt(C_I(I_SetAlias((AliasMode.Plus, bind_aid), cmds)));
        Queue.enqueue keybind_toplevel @@
          Cstmt(C_I(I_Bind(k, [Cstmt(C_I(I_Call(AliasMode.Plus, bind_aid)))])))
  );

  let keybind_alias_types = 
    AliasId.Map.of_alist_exn @@ Queue.to_list keybind_alias_types in

  {
    c_var_domains = modifier_var_domains;
    c_var_initial_values = modifier_var_initial_values;
    c_alias_types = merge_map_exn modifier_alias_types keybind_alias_types;
    c_toplevel = modifier_toplevel @ Queue.to_list keybind_toplevel;
  }

let parse_tree_to_cprog parse_tree = 
  let (decls, bound_tree) = bind_names parse_tree in
  let types = typecheck bound_tree in
  with_errors (fun error ->
    let prog1, modifier_keys, keybinds = eval_bound_program decls types bound_tree error in
    let prog2 = convert_keybinds modifier_keys keybinds error in
    check_cprog {
      c_var_domains        = merge_map_exn prog1.c_var_domains        prog2.c_var_domains;
      c_var_initial_values = merge_map_exn prog1.c_var_initial_values prog2.c_var_initial_values;
      c_alias_types        = merge_map_exn prog1.c_alias_types        prog2.c_alias_types;
      c_toplevel           = List.append   prog1.c_toplevel           prog2.c_toplevel;
    }
  )
