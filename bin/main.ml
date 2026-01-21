let () =
  let lex = Ocamlr.Lexer.{ source = ": () a1bbv32 * + | \"bbakgnfkemk\""; pos = 0 } in
  let rec loop ilex = 
    let ilex, tok = Ocamlr.Lexer.next_token ilex in
    print_endline (Ocamlr.Token.show_token tok);
    match tok with
    | Ocamlr.Token.Eof -> ()
    | _ -> loop ilex
  in
  loop lex
