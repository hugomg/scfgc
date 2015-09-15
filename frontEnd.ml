open Core_kernel.Std
open Types

(* ----------- *)

module NameId = Id.Make( )

type expr = NameId.t Interpolation.t

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

let bind_names parse_tree = with_errors (fun error ->

  let decltable = NameId.Table.create () in

  let noone = NameId.fresh() in (* to keep chugging along after finding an error *)
  
  let rec bind symbol_table (ParseTree(pos, stmt)) =

    let lookup_name pos name =
      match List.Assoc.find symbol_table name with
      | Some nid -> nid
      | None -> error pos (sprintf "Undefined name %s" name); noone
    in

    let interpolate_string pos raw_str =
      Interpolation.of_string raw_str |>
      Interpolation.map ~f:(fun name -> lookup_name pos name)
    in
 
    let do_id (pos, name) =
      (pos, lookup_name pos name)
    in

    let do_expr (pos, name) =
      (pos, interpolate_string pos name)
    in

    let do_cmd (pos, name) =
      (* Commands with no quotes and no interpolations
       * are potentially an alias call *)
      let cmd =
        match name with
        | Either.Second s -> Cmd_Dyn (interpolate_string pos s)
        | Either.First s ->
          let ip = interpolate_string pos s in
          match ip with
          | Interpolation.I(s, []) -> Cmd_Const s
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
      let Interpolation.I(_, xs) = expr in
      List.iter xs ~f:(fun (_,_,nid) ->
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

type eval_value =
  | V_String of string
  | V_VarId of VarId.t
  | V_AliasId of AliasId.t
  | V_Func

let eval_bound_program
  (decls : bound_decl NameId.Map.t)
  (types : name_type NameId.Map.t)
  (bound_tree : bound_tree)
  = with_errors (fun error ->

  let keybinds = Queue.create () in

  let q_var_initial_values = Queue.create () in
  let q_var_domains = Queue.create () in
  let q_alias_types = Queue.create () in
  let q_alias_defs = Queue.create () in
  let q_events = Queue.create () in
  let q_binds = Queue.create () in

  (* --- *)

  let domain_to_int xs = 
    List.mapi xs ~f:(fun i s -> (s, i)) in

  let get_domain nid =
    match Map.find_exn decls nid with
    | Decl_Var(domain_strings, _) ->
        domain_to_int domain_strings
    | _ -> assert false
  in

     let get_val pv name =
      List.Assoc.find_exn ~equal:NameId.equal pv name
    in
    let get_string pv name = 
      match get_val pv name with
      | V_String x -> x | _ -> assert false
    in
    let get_varid pv name = 
      match get_val pv name with
      | V_VarId x -> x | _ -> assert false
    in
    let get_aliasid pv name = 
      match get_val pv name with
      | V_AliasId x -> x | _ -> assert false
    in

 
  
  let rec eval
    (is_top:bool)
    (param_values: (NameId.t * eval_value) list)
    (BoundTree(startpos, symbol_table, stmt))
  =

    let interpolate pv ii =
      Interpolation.map ii ~f:(get_string pv) |>
      Interpolation.to_string
    in

    let do_expr pv (_, ip) =
      interpolate pv ip
    in

    let do_cmd pv (_, cmd) =
      match cmd with
      | Cmd_Dyn expr ->
          (CmdB_Primitive, interpolate pv expr)
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
 
    let param_values =
      match stmt with
      | Let ((_,nid), decl, _) ->
        let param =
          match decl with
          | Decl_Const expr -> V_String (do_expr param_values expr)
          | Decl_Alias _ -> V_AliasId (AliasId.fresh())
          | Decl_Var _ -> V_VarId (VarId.fresh())
          | Decl_Macro _ -> V_Func
        in
        (nid, param)::param_values
      | _ -> param_values
    in

    let stmt_with_children_converted
      : (NameId.t, cmd_bind * string,  string, string, cstmt list) sstmtf
    =
      match stmt with
      | Let ((_, nid), decl, scope) -> (
        let scope' = eval is_top param_values scope in
        let rec_decl = 
          match decl with
          | Decl_Alias(_) -> eval false param_values
          | Decl_Macro _ -> (fun _ -> [])
          | _ -> (fun _ -> assert false)
        in
        let decl' =
          map_decl decl
            ~id:snd
            ~expr:(do_expr param_values)
            ~str:snd
            ~t:rec_decl
        in
        Let(nid, decl', scope') )
      | Block(_) ->
        map_sstmtf stmt
          ~id:snd
          ~cmd:(do_cmd param_values)
          ~expr:(do_expr param_values)
          ~str:snd
          ~t:(eval is_top param_values)
      | _ ->
        map_sstmtf stmt
          ~id:snd
          ~cmd:(do_cmd param_values)
          ~expr:(do_expr param_values)
          ~str:snd
          ~t:(eval false param_values)
    in

    match stmt_with_children_converted with
    | Nop -> []
    | Block stmts -> List.concat stmts
    | Cmd((cmdtyp, cmd), args) -> (
        match cmdtyp with
        | CmdB_Primitive ->
            [ Cstmt(C_Do(cmd, args)) ]
        | CmdB_Alias anid ->
            if not (List.is_empty args) then error startpos "Calling an alias with parameters";
            let aid = get_aliasid param_values anid in
            [ Cstmt(C_Call(AliasMode.None, aid)) ])
    | Let(nid, decl, scope) -> (
        (match decl with
        | Decl_Var(domain, init) -> (
            let var_id = get_varid param_values nid in
            let di = domain_to_int domain in
            let iinit = List.Assoc.find_exn di init in
            Queue.enqueue q_var_domains (var_id, List.length di);
            Queue.enqueue q_var_initial_values (var_id, iinit);
            ())
        | Decl_Alias(body) -> (
            let aid = get_aliasid param_values nid in
            Queue.enqueue q_alias_types (aid, ASimple);
            Queue.enqueue q_alias_defs ((AliasMode.None, aid), body);
            ())
        | _ -> ());
        scope )
    | CallMacro(nid, params) -> (
        let param_nids, body =
          match (Map.find_exn decls nid) with
          | Decl_Macro(param_nids, body) -> (param_nids, body)
          | _ -> assert false
        in
        let params' = List.map2_exn  param_nids params  ~f:(fun nid s -> (nid, V_String s)) in
        eval is_top (List.rev_append params' param_values) body )
    | Assign(nid, value) -> (
        if is_top then
          (error startpos "Variable assignment at toplevel"; [])
        else
          let var_id = get_varid param_values nid in
          let domain = get_domain nid in
          let ivalue = List.Assoc.find_exn domain value ~equal:String.equal in
          [ Cstmt(C_SetVar(var_id, ivalue)) ] )
    | Increment(nid) -> (
        let var_id = get_varid param_values nid in
        let domain = get_domain nid in
        let n = List.length domain in
        let cases = List.map domain ~f:(fun (_, i) -> [Cstmt(C_SetVar(var_id, (i+1)%n))]) in
        [ Cstmt(C_Cond(var_id, Array.of_list cases)) ] )
    | Cond(ct, nid, cases) -> (
        let var_id = get_varid param_values nid in
        let domain = get_domain nid in
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

        let cond_cmd = Cstmt(C_Cond(var_id, Array.of_list ifcases)) in
        
        match ct with
        | Cond_If -> (
          [ cond_cmd ])
        | Cond_When -> (
          if not is_top then error startpos "When statement not at toplevel (use if instead)";
          let aid = AliasId.fresh () in
          let aref = (AliasMode.None, aid) in
          Queue.enqueue q_alias_types (aid, ASimple);
          Queue.enqueue q_alias_defs (aref, [ cond_cmd ]);
          Queue.enqueue q_events (var_id, aref);
          [ Cstmt(C_Call(aref) )] )
        )
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
              | Cstmt(C_Do(cmdname, _)) ->
                Some cmdname
              | Cstmt(C_Call(amode, aid)) -> (
                assert (amode = AliasMode.None);
                match 
                  List.find param_values ~f:(fun (_, value) ->
                    match value with
                    | V_AliasId x when x = aid -> true
                    | _ -> false )
                with
                | None -> None (* found a just-created alias from an if statement *)
                | Some (plus_nid, _)->
                  Some (fst @@ List.find_exn symbol_table 
                          ~f:(fun (_name, nid) -> plus_nid = nid)) )
              | _ ->
                None
            in

            let onkeydown =
              match mplus_cmdname with
              | None -> []
              | Some plus_cmdname -> (
                  if plus_cmdname.[0] <> '+' then
                    []
                  else
                    let minus_cmdname = "-" ^ String.drop_prefix plus_cmdname 1 in
                    let minus_cmd =
                      match plus_cmd with
                      | Cstmt(C_Do(_, args)) ->
                          Cstmt(C_Do(minus_cmdname, args))
                      | Cstmt(C_Call(amode, _)) -> (
                          assert (amode = AliasMode.None);
                          match
                            List.Assoc.find symbol_table minus_cmdname
                              ~equal:String.equal
                          with
                          | None ->
                              error startpos (sprintf "Alias %s not in scope" minus_cmdname);
                              plus_cmd
                          | Some minus_nid -> 
                            let minus_aid = get_aliasid param_values minus_nid in
                            Cstmt(C_Call(amode, minus_aid) ))
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

  let convert_keybinds (keybinds : cstmt keybind list) =

    if
      (* Has duplicate keybinds? *)
      keybinds |>
      List.map ~f:(fun b -> 
          (* " " was our separator back in source code form so its OK to reuse here... *)
          let s = String.concat ~sep:" " (b.b_key :: b.b_modifiers) in (s, b) ) |>
      String.Map.of_alist_fold ~init:[] ~f:(fun xs x -> x :: xs) |>
      Map.data |>
      List.map ~f:(function
          | [] -> assert false
          | [_] -> false
          | bs -> (
              List.iter bs ~f:(fun b ->
                  let keycombo = String.concat ~sep:"+" (b.b_modifiers @ [b.b_key]) in
                  error b.b_pos (sprintf "Conflicting keybinds for %s" keycombo) );
              true)
        ) |>
      List.fold_left ~init:false ~f:((||))
    then
      ()
    else (

      let mod_keys binds = 
        binds |>
        List.map ~f:(fun b -> String.Set.of_list b.b_modifiers) |>
        String.Set.union_list |>
        String.Set.to_list
      in

      let modifier_variables =
        mod_keys keybinds |>
        List.map  ~f:(fun m ->
          let varid = VarId.fresh () in
          Queue.enqueue q_var_domains (varid, 2);
          Queue.enqueue q_var_initial_values (varid, 0);
          (m, varid)
          ) |>
        String.Map.of_alist_exn in

      let keybinds_by_key = 
        List.fold_left
          keybinds
          ~init: (Map.map modifier_variables ~f:(fun _ -> []))
          ~f:(fun m b ->
            Map.add_multi m ~key:b.b_key ~data:b)
      in

      (* Group keybinds by what modifier keys they use *)
      let casify binds cb =
        let rec go used_mods unsorted_mods binds = 
          match unsorted_mods with
          | [] -> (
              match binds with
              | (_::_::_) -> assert false
              | [] -> []
              | [{b_onkeydown; b_onkeyup;_}] ->
                cb b_onkeydown b_onkeyup
            )
          | (m :: ms) ->
            let (pressed, unpressed) =
              List.partition_tf binds ~f:(fun {b_modifiers;_} ->
                  List.mem ~equal:String.equal b_modifiers m )
            in
            let v = Map.find_exn modifier_variables m in
            [Cstmt(C_Cond(v, [|
                 go     used_mods  ms unpressed;
                 go (m::used_mods) ms pressed;
               |]))]
        in
        go [] (mod_keys binds) binds
      in

      Map.iter keybinds_by_key ~f:(fun ~key:k ~data:binds ->
          (* If we have any statements that should be run when a key is released
           * then we must make sure that we only ever call "bind" once, using indirection
           * through an alias for the multiple cases. If we call "bind" again in the middle
           * of a "bind k +foo" call, the system forgets the old bind and doesn't call the
           * "-foo" on release. This can happen when you release one of the modifier keys
           * before you release the main key *)

          let is_modifier_key = Map.mem modifier_variables k in
          let has_onkeydown_binds = List.exists binds ~f:(fun b -> not (List.is_empty b.b_onkeyup)) in

          if (not is_modifier_key) && (not has_onkeydown_binds) then (
            (* as an optimization, don't create the "+" aliases here *)
            let aid = AliasId.fresh () in
            let aref = (AliasMode.None, aid) in
            let cmds = casify binds (fun onkeydown _onkeyup -> onkeydown) in
            Queue.enqueue q_alias_types (aid, ASimple);
            Queue.enqueue q_alias_defs (aref, cmds);
            Queue.enqueue q_binds (k, aref)
          ) else (

            let aid = AliasId.fresh() in
            let arefp = (AliasMode.Plus, aid) in
            let arefm = (AliasMode.Minus, aid) in

            let cb_var = 
              if has_onkeydown_binds then
                Some(VarId.fresh (), Queue.create ())
              else
                None
            in
            
            let (mod_down, mod_up) = 
              match Map.find modifier_variables k with
              | None -> ([], [])
              | Some v -> ([Cstmt(C_SetVar(v, 1))], [Cstmt(C_SetVar(v, 0))])
            in

            let cmdsp = casify binds (fun onkeydown onkeyup ->
              onkeydown @ match cb_var with
                | None -> []
                | Some (v, queue) -> (
                    let n = Queue.length queue in
                    Queue.enqueue queue onkeyup;
                    [ Cstmt(C_SetVar(v, n)) ] ) )
            in

            let cmdsm =
              match cb_var with
              | None -> []
              | Some(v, q) ->
                [ Cstmt(C_Cond(v, Queue.to_array q)) ]
            in

            (match cb_var with
              | None -> ()
              | Some(v, q) -> (
                Queue.enqueue q_var_domains (v, Queue.length q);
                Queue.enqueue q_var_initial_values (v, 0); (* any value is ok *)
              ));

            Queue.enqueue q_alias_types (aid, APlusMinus);
            Queue.enqueue q_alias_defs (arefp, mod_down @ cmdsp);
            Queue.enqueue q_alias_defs (arefm, mod_up @ cmdsm);
            Queue.enqueue q_binds (k, arefp)
          )
        );

      ()
    )

  in

  let toplevel = eval true [] bound_tree in
  convert_keybinds (Queue.to_list keybinds); 

  check_cprog {
    c_var_initial_values = VarId.Map.of_alist_exn @@ Queue.to_list q_var_initial_values;
    c_var_domains        = VarId.Map.of_alist_exn @@ Queue.to_list q_var_domains;
    c_alias_types      = AliasId.Map.of_alist_exn @@ Queue.to_list q_alias_types;
    c_alias_defs      = AliasRef.Map.of_alist_exn @@ Queue.to_list q_alias_defs;
    c_binds             = String.Map.of_alist_exn @@ Queue.to_list q_binds;
    c_events = Queue.to_list q_events;
    c_toplevel = toplevel;
  }
)

let parse_tree_to_cprog parse_tree = 
  let (decls, bound_tree) = bind_names parse_tree in
  let types = typecheck bound_tree in
  eval_bound_program decls types bound_tree
