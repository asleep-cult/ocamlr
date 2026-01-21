type t = { source: string; pos: int }

exception Unterminated_string
exception Invalid_token

let eof = '\x00'

let is_whitespace = function
  | ' ' | '\t' | '\r' -> true
  | _ -> false

let is_identifier_start = function
  | 'A'..'Z' | 'a'..'z' | '_' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_identifier c = is_identifier_start c || is_digit c

let peek_char lex i =
  if String.length lex.source > lex.pos+i then lex.source.[lex.pos+i]
  else eof

let advance lex i = { lex with pos = lex.pos+i }

let rec advance_while pred lex = 
  if pred (peek_char lex 0) then advance_while pred { lex with pos = lex.pos+1}
  else lex

let identifier_token lex =
  let pos = lex.pos in 
  let lex = advance_while is_identifier lex in
  lex, Token.Identifier (String.sub lex.source pos (lex.pos-pos))

let string_token lex =
  let lex = advance lex 1 in
  let pos = lex.pos in
  let lex = advance_while (
    function
      | c when c = '\n' || c = eof -> raise Unterminated_string
      | '"' -> false
      | _ -> true) lex in
  advance lex 1, Token.String (String.sub lex.source pos (lex.pos-pos))

let next_token lex =
  let lex = advance_while is_whitespace lex in
  match peek_char lex 0 with
    | '(' -> advance lex 1, Token.OpenParenthesis
    | ')' -> advance lex 1, Token.CloseParanthesis
    | '"' -> string_token lex
    | '+' -> advance lex 1, Token.Plus
    | '*' -> advance lex 1, Token.Star
    | '\'' -> advance lex 1, Token.Tick
    | '?' -> advance lex 1, Token.Question
    | '|' -> advance lex 1, Token.VerticalBar
    | ':' -> advance lex 1, Token.Colon
    | c when is_identifier_start c -> identifier_token lex
    | c when c = eof -> lex, Token.Eof
    | _ -> raise Invalid_token
