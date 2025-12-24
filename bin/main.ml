let () =
  let lex = Ocamlr.Lexer.{ source = "1234 : () a1b2 * + | : .. \""; pos = 0 } in
  let rec loop ilex = 
    let ilex, tok = Ocamlr.Lexer.next_token ilex in
    print_endline (Ocamlr.Token.token_name tok);
    match tok with
    | Ocamlr.Token.Eof -> ()
    | _ -> loop ilex
  in
  loop lex
