type t =
  | OpenParenthesis
  | CloseParanthesis
  | Quote
  | Star
  | Plus
  | VerticalBar
  | Colon
  | DoubleDot
  | Identifier
  | Number
  | Eof
  | Invalid

let token_name = function
  | OpenParenthesis -> "OpenParenthesis"
  | CloseParanthesis -> "CloseParanthesis"
  | Quote -> "Quote"
  | Star -> "Star"
  | Plus -> "Plus"
  | VerticalBar -> "VerticalBar"
  | Colon -> "Colon"
  | DoubleDot -> "DoubleDot"
  | Identifier -> "Identifier"
  | Number -> "Number"
  | Eof -> "Eof"
  | Invalid -> "Invalid"
