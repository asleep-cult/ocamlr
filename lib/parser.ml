type t = { tok_table: (string, int) Hashtbl.t; lex: Lexer.t; toks: Token.t list }

exception Invalid_syntax of string

let peek_token parser i =
  let rec aux parser =
    if List.length parser.toks >= i then parser
    else
      let lex, tok = Lexer.next_token parser.lex in
      aux { parser with lex = lex; toks = (parser.toks @ [tok]) }
    in
  let parser = aux parser in
  parser, List.nth parser.toks (i-1)

let bump_parser parser =
  match parser.toks with
  | _ :: rest -> { parser with toks = rest }
  | [] -> parser

let next_token parser =
  let parser, tok = peek_token parser 1 in
  bump_parser parser, tok

let parse_atom parser =
  let pos = parser.lex.pos in
  let parser, tok = next_token parser in
  let id = match tok with
    | Token.String(cont)
    | Token.Identifier(cont) -> Hashtbl.find parser.tok_table cont
    | _ -> raise (Invalid_syntax (Token.show_token tok))
  in
  parser, Ast.{
    span = { startpos = pos; endpos = parser.lex.pos };
    data = Token { id = id } }

let parse_suffix parser expr =
  let parser, tok = peek_token parser 1 in
  let parser, data = match tok with
    | Token.Plus -> bump_parser parser,
        Ast.Plus { expr = expr }
    | Token.Star -> bump_parser parser,
        Ast.Star { expr = expr }
    | Token.Question -> bump_parser parser,
        Ast.Option { expr = expr }
    | _ -> parser, expr.data
  in
  parser, Ast.{
    span = { startpos = expr.span.startpos; endpos = parser.lex.pos};
    data = data }

let rec parse_expr_list parser =
  let rec aux parser exprs =
    let parser, tok = peek_token parser 1 in
    match tok with
      | Token.OpenParenthesis
      | Token.Identifier(_)
      | Token.String(_) ->
        let parser, expr = parse_group_item parser in
        aux parser (expr :: exprs)
      | _ -> parser, exprs
  in
  let parser, exprs = aux parser [] in
  parser, List.rev exprs

and parse_group parser =
  let pos = parser.lex.pos in
  let parser, exprs = parse_expr_list parser in
  parser, Ast.{ span = { startpos = pos; endpos = parser.lex.pos };
    data = Ast.Group { exprs = exprs } }

and parse_group_item parser = 
  let parser, tok = peek_token parser 1 in
  let parser, expr = match tok with
    | Token.OpenParenthesis ->
      let parser = bump_parser parser in
      let parser, expr = parse_group parser in
      let parser, tok = next_token parser in
      (match tok with
        | Token.CloseParanthesis -> parser, expr
        | _ -> raise (Invalid_syntax (Token.show_token tok)))
    | _ -> parse_atom parser
  in
  parse_suffix parser expr

let parse_rule parser =
  let pos = parser.lex.pos in
  let parser, tok = next_token parser in
  let rule_name = match tok with
    | Token.Identifier(cont) -> cont
    | _ -> raise (Invalid_syntax (Token.show_token tok))
  in
  let parser, tok = peek_token parser 1 in
  let parser, is_entrypoint = match tok with
    | Token.Tick -> bump_parser parser, true
    | _ -> parser, false
  in
  let parser, tok = next_token parser in
  let () = match tok with  (* um ... *)
    | Token.Colon -> ()
    | _ -> raise (Invalid_syntax (Token.show_token tok))
  in
  let parser, tok = peek_token parser 1 in
  let parser = match tok with
    | Token.VerticalBar -> bump_parser parser
    | _ -> parser
  in
  let rec aux parser items =
    let pos = parser.lex.pos in
    let parser, exprs = parse_expr_list parser in
    let parser, tok = peek_token parser 1 in
    let parser, action = match tok with
      | Token.Arrow ->
        let parser = bump_parser parser in
        let parser, tok = next_token parser in
        parser, (match tok with
          | Token.Identifier(cont) -> Some cont
          | _ -> raise (Invalid_syntax (Token.show_token tok)))
      | _ -> parser, None
    in
    let items = Ast.{
      span = { startpos = pos; endpos = parser.lex.pos };
      exprs = exprs;
      action = action } :: items in
    let parser, tok = peek_token parser 1 in
    match tok with
      | Token.VerticalBar ->
        let parser = bump_parser parser in
        aux parser items
      | _ -> parser, items
  in
  let parser, items = aux parser [] in
  parser, Ast.{
    span = { startpos = pos; endpos = parser.lex.pos };
    name = rule_name; is_entrypoint = is_entrypoint; items = items }

let parse_rules parser =
  let rec aux parser rules =
    let parser, tok = peek_token parser 1 in
    match tok with
    | Token.Eof -> parser, rules
    | _ ->
      let parser, rule = parse_rule parser in
      aux parser (rule :: rules)
  in
  let _, rules = aux parser [] in List.rev rules
