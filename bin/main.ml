let () =
  let tok_table = Hashtbl.create 1 in
  let () = Hashtbl.add tok_table "+" 0 in
  let lex = Ocamlr.Lexer.{ source = "nonterminal:\n(\"+\" \"+\")*  => create_plus | \"+\"* \"+\"? => other_plus"; pos = 0 } in
  let parser = Ocamlr.Parser.{ lex = lex; tok_table = tok_table; toks = [] } in
  Printexc.record_backtrace true;
  let rec aux = function
    | head :: rest ->
      print_endline (Ocamlr.Ast.show_rule head);
      aux rest
    | [] -> ()
  in
  aux (Ocamlr.Parser.parse_rules parser)
