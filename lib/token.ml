type t =
  | String of string
  | Identifier of string
  | Arrow
  | OpenParenthesis
  | CloseParanthesis
  | Star
  | Plus
  | Tick
  | Question
  | VerticalBar
  | Colon
  | Eof

let show_token = function
  | String(cont) -> Printf.sprintf "String(cont: %s)" cont
  | Identifier(cont) -> Printf.sprintf "Identifier(cont: %s)" cont
  | Arrow -> "Arrow"
  | OpenParenthesis -> "OpenParenthesis"
  | CloseParanthesis -> "CloseParanthesis"
  | Star -> "Star"
  | Plus -> "Plus"
  | Tick -> "Tick"
  | Question -> "Question"
  | VerticalBar -> "VerticalBar"
  | Colon -> "Colon"
  | Eof -> "Eof"
