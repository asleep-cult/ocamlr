type t = { tok_table: (string, int) Hashtbl.t; lex: Lexer.t; toks: Token.t list }

exception Invalid_syntax

let peek_token parser i =
  let rec aux parser =
    if List.length parser.toks >= i then parser
    else
      let lex, tok = Lexer.next_token parser.lex in
      aux { parser with lex = lex; toks = tok :: parser.toks }
    in
  let parser = aux parser in
  parser, List.nth parser.toks (List.length parser.toks - i)

let bump_parser parser =
  match parser.toks with
  | _ :: rest -> { parser with toks = rest }
  | [] -> parser

let next_token parser =
  let parser, tok = peek_token parser 1 in
  bump_parser parser, tok

let parse_rule parser =
  let pos = parser.lex.pos in
  let parser, tok = next_token parser in
  let rule_name = match tok with
    | Token.Identifier(cont) -> cont
    | _ -> raise Invalid_syntax
  in
  let parser, tok = peek_token parser 1 in
  let parser, is_entrypoint = match tok with
    | Token.Tick -> bump_parser parser, true
    | _ -> parser, false
  in
  parser, Ast.{ span = (pos, parser.lex.pos); name = rule_name; is_entrypoint = is_entrypoint; items = [] }
