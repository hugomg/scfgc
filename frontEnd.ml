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

let plus_alias_re =
  let open Re2.Std in
  Re2.create_exn "^\\+"

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

  let keybind_modifiers = String.Hash_set.create () in
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

    let interpolate pv (Interpolation(nids, func)) =
      func (List.map nids ~f:(get_string pv ))
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
            List.iter mods ~f:(Hash_set.add keybind_modifiers);
            if Hash_set.mem keybind_modifiers key then
              error
                startpos
                (sprintf "Cannot use %s as a keybind - its already used as a modifier" key)
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

  let convert_keybinds 
    (mod_keys : string list)
    (keybinds : cstmt keybind list)
    =

      let modifier_variables =
        mod_keys |>
        List.map  ~f:(fun m -> (m, VarId.fresh ())) |>
        String.Map.of_alist_exn in
     
      List.iter mod_keys ~f:(fun m ->
        let varid = Map.find_exn modifier_variables m in
        Queue.enqueue q_var_domains (varid, 2);
        Queue.enqueue q_var_initial_values (varid, 0);

        let aid = AliasId.fresh () in
        Queue.enqueue q_alias_types (aid, APlusMinus);
        Queue.enqueue q_alias_defs ((AliasMode.Plus,  aid), [ Cstmt(C_SetVar(varid, 1)) ]);
        Queue.enqueue q_alias_defs ((AliasMode.Minus, aid), [ Cstmt(C_SetVar(varid, 0)) ]);
        Queue.enqueue q_binds (m, (AliasMode.Plus, aid))
      );

      (* Group keybinds by what modifier keys they use *)
      let rec casify used_mods unsorted_mods cb =
        match unsorted_mods with
        | [] -> cb used_mods 
        | (m :: ms) ->
          let v = Map.find_exn modifier_variables m in
          [Cstmt(C_Cond(v, [|
               casify     used_mods  ms cb;
               casify (m::used_mods) ms cb;
             |]))]
      in

      (* If we have any statements that should be run when a key is released
       * then we must make sure that we only ever call "bind" once, using indirection
       * through an alias for the multiple cases. If we call "bind" again in the middle
       * of a "bind k +foo" call, the system forgets the old bind and doesn't call the
       * "-foo" on release. This can happen when you release one of the modifier keys
       * before you release the main key *)


      let keybinds_by_key = 
        keybinds |>
        List.map ~f:(fun b -> (b.b_key, b)) |>
        String.Map.of_alist_fold ~init:[] ~f:(fun bs b -> b :: bs) in

      let key_alias = 
        Map.map keybinds_by_key ~f:(fun bs ->
          if List.for_all bs ~f:(fun b -> List.is_empty b.b_onkeyup) then
            Either.Left (AliasId.fresh (), Queue.create())
          else
            Either.Right (AliasId.fresh (), VarId.fresh(), Queue.create(), Queue.create() ) ) in

      let state_var = VarId.fresh () in
      let next_stateid = ref 0 in

      let bigif = casify [] mod_keys (fun used_mods ->
        let stateid = !next_stateid in
        next_stateid := stateid + 1;
        Map.iter keybinds_by_key ~f:(fun ~key:k ~data:bs ->
          let bs = List.filter bs ~f:(fun b ->
            String.Set.equal
              (String.Set.of_list b.b_modifiers)
              (String.Set.of_list used_mods)) in

          let onkeydown, onkeyup = match bs with
            | [] -> ([], [])
            | [b] -> (b.b_onkeydown, b.b_onkeyup)
            | bs -> (
                List.iter bs ~f:(fun b ->
                    let keycombo = String.concat ~sep:"+" (used_mods @ [b.b_key]) in
                    error b.b_pos (sprintf "Conflicting keybinds for %s" keycombo) );
                ([], []) ) in

          match Map.find_exn key_alias k with
          | Either.Left (_, cases) ->
            Queue.enqueue cases onkeydown 
          | Either.Right(_, vid, downq, upq) -> (
              Queue.enqueue downq (onkeydown @ [Cstmt(C_SetVar(vid, stateid))]);
              Queue.enqueue upq    onkeyup)
          );
        [ Cstmt(C_SetVar(state_var, stateid)) ]
        ) in

      let state_alias = AliasId.fresh () in
      let state_ref = (AliasMode.None, state_alias) in
      Queue.enqueue q_alias_types (state_alias, ASimple);
      Queue.enqueue q_alias_defs (state_ref, bigif);

      Map.iter modifier_variables ~f:(fun ~key:_ ~data:mvar ->
        Queue.enqueue q_events (mvar, state_ref) );

      Queue.enqueue q_var_initial_values (state_var, 0);
      Queue.enqueue q_var_domains (state_var, !next_stateid);

      Map.iter key_alias ~f:(fun ~key:k ~data:d ->
        let aref = match d with
        | Either.Left(aid, cases) -> (
          let aref = (AliasMode.None, aid) in
          Queue.enqueue q_alias_types (aid, ASimple);
          Queue.enqueue q_alias_defs (aref, [Cstmt(C_Cond(state_var, Queue.to_array cases))]);
          aref)
        | Either.Right(aid, cb_var, downq, upq) -> (
          let arefp = (AliasMode.Plus, aid) in
          let arefm = (AliasMode.Minus, aid) in
          Queue.enqueue q_var_initial_values (cb_var, 0);
          Queue.enqueue q_var_domains (cb_var, !next_stateid);
          Queue.enqueue q_alias_types (aid, APlusMinus);
          Queue.enqueue q_alias_defs (arefp, [Cstmt(C_Cond(state_var, Queue.to_array downq)) ]);
          Queue.enqueue q_alias_defs (arefm, [Cstmt(C_Cond(cb_var, Queue.to_array upq)) ]);
          arefp)
        in
        Queue.enqueue q_binds (k, aref) );

      ()

  in

  let toplevel = eval true [] bound_tree in
  convert_keybinds (Hash_set.to_list keybind_modifiers) (Queue.to_list keybinds); 

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
