type span = int * int
and rule = { span: span; name: string; is_entrypoint: bool; items: rule_item list }
and rule_item = { span: span; exprs: expr list; action: string }

and expr =
| Star of { span: span; expr: expr }
| Plus of { span: span; expr: expr }
| Option of { span: span; expr: expr }
| Alternative of { span: span; lhs: expr; rhs: expr }
| Token of { span: span; id: int }
| Name of { span: span; cont: string }
