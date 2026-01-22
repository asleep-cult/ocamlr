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
  | _ :: rest -> { parser with toks = rest } (* I actually want everything but the last item... ? *)
  | [] -> parser (* I think i could get rid of this logic but then we're lexing the same thing over and over again? *)

let next_token parser =
  let parser, tok = peek_token parser 1 in
  bump_parser parser, tok

let parse_atom parser =
  let pos = parser.lex.pos in
  let parser, tok = next_token parser in
  let id = match tok with
    | Token.String(cont)
    | Token.Identifier(cont) -> Hashtbl.find parser.tok_table cont
    | _ -> raise Invalid_syntax
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

let parse_group parser =
  parse_atom parser
let parse_group_item parser = 
  let parser, tok = next_token parser in
  let parser, expr = match tok with
    | Token.OpenParenthesis ->
      let parser = bump_parser parser in
      let parser, expr = parse_group parser in
      let parser, tok = next_token parser in
      (* Why can't I match on tok and raise an exception... *)
      parser, expr
    | _ -> parse_atom parser
  in
  parse_suffix parser expr

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
  parser, Ast.{
    span = Ast.{ startpos = pos; endpos = parser.lex.pos };
    name = rule_name; is_entrypoint = is_entrypoint; items = [] }
