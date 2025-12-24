type t = { source: string; pos: int }

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

let identifier_token lex = advance_while is_identifier_start lex

let next_token lex =
  let lex = advance_while is_whitespace lex in
  match peek_char lex 0 with
    | '(' -> advance lex 1, Token.OpenParenthesis
    | ')' -> advance lex 1, Token.CloseParanthesis
    | '"' -> advance lex 1, Token.Quote
    | '*' -> advance lex 1, Token.Star
    | '+' -> advance lex 1, Token.Plus
    | '|' -> advance lex 1, Token.VerticalBar
    | ':' -> advance lex 1, Token.Colon
    | '.' when peek_char lex 1 == '.' -> advance lex 2, Token.DoubleDot
    | c when is_identifier_start c -> advance_while is_identifier lex, Token.Identifier
    | c when is_digit c -> advance_while is_digit lex, Token.Number
    | c when c = eof -> lex, Token.Eof
    | _ -> lex, Token.Invalid
