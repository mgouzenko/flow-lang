open Ast;;

let node_num = ref 0
let dot_graph = ref ""

let print_dot_node node_desc =
    node_num:= !node_num + 1;
    let node = "{" ^ string_of_int(!node_num) ^ "[label=\"" ^ node_desc ^ "\"]};" in
    dot_graph := !dot_graph ^ node; !node_num;;

let print_dot_edge node1 node2 =
    dot_graph := !dot_graph ^ (string_of_int(node1) ^ "->" ^ string_of_int(node2) ^ ";")

let print_string_of_binop op =
    let opstring = match op with
	    Plus  -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Divide -> "/"
      | Modulo -> "%"
      | Eq -> "=="
      | Neq -> "!="
      | Lt -> "<"
      | Leq -> "<="
      | Gt -> ">"
      | Geq -> ">="
      | Send -> "->"
      | And -> "and"
      | Or  -> "or"
      | Assign -> "="
          in print_dot_node opstring;;

let print_string_of_unop op =
    let opstring = match op with
          Retrieve -> "@"
        | Negate   -> "-"
        | Not      -> "1" in
    print_dot_node opstring

let rec print_string_of_expr = function
    IntLiteral(l) -> print_dot_node(string_of_int l)
  | StringLiteral(s) -> print_dot_node(s)
  | BoolLiteral(b) -> print_dot_node(string_of_bool b)
  | CharLiteral(c) -> print_dot_node (Char.escaped c)
  | DoubleLiteral(d) -> print_dot_node "Some double"
  | StructInitializer(decl_list) -> print_dot_node "StructInitializer"
  | Id(s) -> print_dot_node s
  | BinOp(e1, op, e2) ->
          let num1 = print_string_of_expr e1
          and num2 = print_string_of_expr e2
          and opnum = print_string_of_binop op in
          let _ = print_dot_edge opnum num1
          and _ = print_dot_edge opnum num2  in
          opnum
  | UnaryOp(op, e) -> let num = print_string_of_expr e
                      and op = print_string_of_unop op
                      in let _ = print_dot_edge op num in op
  | Assign(id, e) -> let node1 = print_dot_node id
                     and node2 = print_string_of_expr e
                     and op = print_dot_node "="
                     in let _ = print_dot_edge op node1
                     and _ = print_dot_edge op node2
                     in op
  | FunctionCall(f, el) ->
          let f_node = print_dot_node f in
          let enodes = List.map print_string_of_expr el in
          let _ = List.iter (function num -> print_dot_edge f_node num) enodes
          in f_node
  | Noexpr -> print_dot_node "Noexpr"

let print_string_of_func_decl fdecl = print_dot_node "Funtion Decl"

let print_string_of_struct_decl sdecl = print_dot_node "Struct Decl"

let print_string_of_var_decl vdecl =
    print_string_of_expr vdecl.declaration_initializer

let print_string_of_decl = function
      VarDecl(vd) -> print_string_of_var_decl vd
    | FuncDecl(fd) -> print_string_of_func_decl fd
    | StructDecl(sd) -> print_string_of_struct_decl sd

let print_string_of_program = function
    Declarations(decls) -> let nodes = List.map print_string_of_decl decls in
                           let program = print_dot_node "Program" in
                           List.iter (function num -> print_dot_edge program num) nodes


type action = Ast | Interpret | Bytecode | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-i", Interpret);
			      ("-b", Bytecode);
			      ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let _ = print_string_of_program program in
  let graph = "digraph G{" ^ !dot_graph ^ "}" in
  Sys.command ("echo \"" ^ graph ^ "\" | dot -Tpng -o out.png")
