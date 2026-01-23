type span = { startpos: int; endpos: int }
and rule = { span: span; name: string; is_entrypoint: bool; items: rule_item list }
and rule_item = { span: span; exprs: expr list; action: string option }

and expr = { span: span; data: expr_data }

and expr_data =
  | Group of { exprs: expr list }
  | Star of { expr: expr }
  | Plus of { expr: expr }
  | Option of { expr: expr }
  | Token of { id: int }
  | Name of { cont: string }

let rec show_expr expr =
  show_expr_data expr.data

and show_expr_list exprs = String.concat ", " (List.map show_expr exprs) 

and show_expr_data = function
  | Group({exprs}) -> Printf.sprintf "Group(%s)" (show_expr_list exprs)
  | Star({expr}) -> Printf.sprintf "Star(%s)" (show_expr expr)
  | Plus({expr}) -> Printf.sprintf "Plus(%s)" (show_expr expr)
  | Option({expr}) -> Printf.sprintf "Option(%s)" (show_expr expr)
  | Token({id}) -> Printf.sprintf "Token(%d)" id
  | Name({cont}) -> Printf.sprintf "Name(%s)" cont

let show_rule_item item =
  let exprs = show_expr_list item.exprs in
  match item.action with
    | Some action -> Printf.sprintf "  rule_item(%s, action=%s)" exprs action
    | None -> Printf.sprintf "  rule_item(%s)" exprs

let show_rule rule =
  let items = String.concat "\n" (List.map show_rule_item rule.items) in
  Printf.sprintf "rule(\n  name=%s\n%s\n)" rule.name items
