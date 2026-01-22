type span = { startpos: int; endpos: int }
and rule = { span: span; name: string; is_entrypoint: bool; items: rule_item list }
and rule_item = { span: span; exprs: expr list; action: string }

and expr = { span: span; data: expr_data }

and expr_data =
  | Group of { exprs: expr list }
  | Star of { expr: expr }
  | Plus of { expr: expr }
  | Option of { expr: expr }
  | Token of { id: int }
  | Name of { cont: string }
