open Core_kernel.Std

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
  let rprog = BackEnd.rprog_of_iprog ~prefix:"ss" iprog in
  BackEnd.print_to_stdout rprog

(* Our lexer can handle UTF-8 encoded files as long as all the non-ASCII characters
 * are inside comments or quoted strings. However, it can't deal with the Byte-Order-Mark
 * that Notepad and some other windows editors might add to the file *)
let skip_utf8_BOM chan =
  let bom = "\xEF\xBB\xBF" in
  let bom_len = String.length bom in
  let orig_pos = In_channel.pos chan in
  let buf = Bytes.make bom_len '\x00' in
  let n = In_channel.input chan ~buf ~pos:0 ~len:bom_len in
  if not (n = bom_len && String.equal bom (Bytes.to_string buf)) then
    In_channel.seek chan orig_pos

let compile in_filename =

  let exit_with_error errname pos errinfo =
    (match pos with
    | None -> ()
    | Some pos -> fprintf stderr "Line %d, column %d:\n" (pos_lnum pos) (pos_cnum pos));
    fprintf stderr "%s" errname;
    (match errinfo with
    | None -> ()
    | Some s -> fprintf stderr ": %s" s);
    fprintf stderr "\n";
    exit 1
  in

  if not (FilePath.check_extension in_filename "scfg") then
    exit_with_error "Error" None
      (Some (sprintf "`%s' does not have a .scfg extension" in_filename))
  ;

  let in_chan =
    try In_channel.create ~binary:false in_filename
    with Sys_error(msg) -> exit_with_error "Error" None (Some msg)
  in
  skip_utf8_BOM in_chan;

  let in_buf = Lexing.from_channel in_chan in
  in_buf.lex_curr_p <- { in_buf.lex_curr_p with pos_fname = in_filename };

  try
    let parse_tree = Parser.prog (Lexer.create()) in_buf in
    let cprog = FrontEnd.parse_tree_to_cprog parse_tree in
    (*print_endline "CPROG =============";
    print_cprog cprog;*)
    let iprog = BackEnd.cprog_to_iprog cprog in
    (*print_endline "IPROG =============";
    print_iprog iprog;*)
    let iprog = BackEnd.inline_constant_aliases iprog in
    (*print_endline "INLINE =============";
    print_iprog iprog;*)
    let iprog = BackEnd.lift_constant_aliases iprog in
    (*print_endline "LIFT =============";
    print_iprog iprog;*)

    (* Now we know there will be no compilation errors so
     * its OK to overwrite any output files *)
    let main_filename = FilePath.replace_extension in_filename "cfg" in
    let outdir = FilePath.chop_extension in_filename in
    let basename = FilePath.basename outdir in

    (* the filename is used to generate internal alias names *)
    if not (String.for_all basename ~f:(fun c -> Char.is_alphanum c || c = '_'))
    then
      exit_with_error "Error" None
        (Some "The name of the input file must contain only letters, numbers and underscores")
    ;

    let (main_lines, helper_files) = BackEnd.rprog_of_iprog iprog ~prefix:basename in

    FileUtil.rm [main_filename];
    FileUtil.rm ~recurse:true [outdir];
    if not (List.is_empty helper_files) then
      FileUtil.mkdir outdir
    ;

    let out_files =
      (main_filename, main_lines) ::
      List.map helper_files ~f:(fun (b, lines) ->
        (FilePath.concat outdir b, lines))
    in

    List.iter out_files ~f:(fun (filename, lines) ->
        Out_channel.with_file ~binary:false filename ~f:(fun file ->
            Out_channel.output_lines file lines
          ));

    ()
  with
    | Lexer.LexerError s ->
        exit_with_error "Lexer Error" (Some in_buf.lex_curr_p) (Some s)
    | Parser.Error ->
        exit_with_error "Syntax Error" (Some in_buf.lex_curr_p) None
    | CompilationError(errs) ->
        List.iter errs ~f:(fun (pos, msg) ->
          exit_with_error "Error" (Some pos) (Some msg))


(* ----- *)
open Cmdliner


let filename =
  let doc = "Source .scfg file" in
  Arg.(required @@ pos 0 (some @@ file) None @@ info [] ~doc ~docv:"filename")

let cmd =
  let doc = "Super Source Config compiler" in
  let man = [] in
  Term.(
    pure compile $ filename,
    info "scfgc" ~version:"0.1" ~doc ~man)

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | `Version | `Help | `Ok _ -> exit 0


