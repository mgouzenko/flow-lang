open Ast;;

let node_num = ref 0
let dot_graph = ref ""

let print_dot_node node_desc =
    node_num:= !node_num + 1;
    let node = "{" ^ string_of_int(!node_num) ^ "[label=\"" ^ node_desc ^ "\"]};\n" in
    dot_graph := !dot_graph ^ node; !node_num;;

let print_dot_edge ?desc:(d="") node1 node2 =
    dot_graph := !dot_graph ^ (  string_of_int(node1) ^ "->"
                               ^ string_of_int(node2)
                               ^ "[label=\"" ^ d ^ "\"];\n")

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
      | Concat -> "::"
          in print_dot_node opstring;;

let print_string_of_unop op =
    let opstring = match op with
          Retrieve -> "@"
        | Negate   -> "-"
        | Not      -> "!" 
        | ListLength -> "#" 
        | ListTail -> "^"
          in
    print_dot_node opstring

let rec print_string_of_expr = function
    IntLiteral(l) -> print_dot_node(string_of_int l)
  | StringLiteral(s) -> print_dot_node(s)
  | BoolLiteral(b) -> print_dot_node(string_of_bool b)
  | CharLiteral(c) -> print_dot_node (Char.escaped c)
  | DoubleLiteral(d) -> print_dot_node "Some double"
  | ListInitializer(expr_list) -> 
          let arrinitnode = print_dot_node "ListInitializer"
          in let enodes = List.map print_string_of_expr expr_list
          in let _ = List.iter (function enode -> print_dot_edge arrinitnode enode) 
                                enodes 
          in arrinitnode
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
  | FunctionCall(f, el) ->
          let f_node = print_dot_node f in
          let enodes = List.map print_string_of_expr el in
          let _ = List.iter (function num -> print_dot_edge f_node num) enodes
          in f_node
  | Noexpr -> print_dot_node "Noexpr"


let rec string_of_type = function
      Int ->  "int"
    | Double -> "double"
    | Bool -> "bool"
    | Char -> "char"
    | Void -> "void"
    | Proc -> "proc"
    | String -> "string"
    | Channel(t, Nodir) -> "channel<" ^ string_of_type t ^ ">"
    | Channel(t, In) -> "in " ^ string_of_type t
    | Channel(t, Out) -> "out " ^ string_of_type t
    | List(t) -> "list<" ^ string_of_type t ^ ">"

let print_string_of_type t =
    print_dot_node (string_of_type t)

let print_string_of_var_decl vdecl =
    let decl_node = print_dot_node "vdecl"
    and type_node = print_string_of_type vdecl.declaration_type
    and name_node = print_dot_node vdecl.declaration_id
    and expr_node = print_string_of_expr vdecl.declaration_initializer
    in let _ = print_dot_edge decl_node type_node ~desc: "type"
    and _ = print_dot_edge decl_node name_node  ~desc: "name"
    and _ = print_dot_edge decl_node expr_node ~desc: "initializer"
    in decl_node

let rec print_string_of_stmt = function
      Expr(e) -> print_string_of_expr e
    | Block(stmt_list) ->
            let block_node = print_dot_node "block" in
            let _ = List.iter (fun stmt ->
                let stmt_node = print_string_of_stmt stmt in
                print_dot_edge block_node stmt_node) stmt_list in
            block_node
    | Return(e) -> print_string_of_expr e
    | Declaration(vdecl) -> print_string_of_var_decl vdecl
    | If(e, s1, s2) ->
            let ifnode = print_dot_node "if"
            and enode  = print_string_of_expr e
            and snode1 = print_string_of_stmt s1
            and snode2 = print_string_of_stmt s2 in
            let _ = print_dot_edge ifnode enode ~desc: "cond"
            and _ = print_dot_edge ifnode snode1 ~desc: "true"
            and _ = print_dot_edge ifnode snode2 ~desc: "false" in
            ifnode
    | For(e1, e2, e3, s) ->
            let fornode = print_dot_node "for"
            and enode1 = print_string_of_expr e1
            and enode2 = print_string_of_expr e2
            and enode3 = print_string_of_expr e3
            and stmt_node = print_string_of_stmt s in
            let _ = print_dot_edge fornode enode1 ~desc: "init"
            and _ = print_dot_edge fornode enode2 ~desc: "cond"
            and _ = print_dot_edge fornode enode3 ~desc: "update"
            and _ = print_dot_edge fornode stmt_node ~desc: "body" in
            fornode
    | While(e, s) ->
            let while_node = print_dot_node "while"
            and enode = print_string_of_expr e
            and snode = print_string_of_stmt s in
            let _ = print_dot_edge while_node enode ~desc: "cond"
            and _ = print_dot_edge while_node snode ~desc: "body"
            in while_node
    | Continue -> print_dot_node "continue"
    | Break -> print_dot_node "break"
    | Poison(chan_id) -> 
            let pnode = print_dot_node "poison"
            and inode = print_string_of_expr chan_id in 
            let _ = print_dot_edge pnode inode
            in pnode

let print_string_of_func_decl fdecl =
    let fdecl_node = print_dot_node "fdecl"
    and type_node = print_string_of_type fdecl.return_type
    and name = print_dot_node fdecl.function_name
    and args_node = print_dot_node "args"
    and args = List.map print_string_of_var_decl fdecl.arguments
    and body = print_dot_node "body" in
    let _ = print_dot_edge fdecl_node type_node ~desc: "type"
    and _ = print_dot_edge fdecl_node name ~desc: "name"
    and _ = print_dot_edge fdecl_node args_node
    and _ = List.iter (fun node -> print_dot_edge args_node node) args
    and _ = print_dot_edge fdecl_node body
    and _ = List.iter (fun stmt ->
                let stmt_node = print_string_of_stmt stmt in
                print_dot_edge body stmt_node) fdecl.body in
    fdecl_node

let print_string_of_decl = function
      VarDecl(vd) -> print_string_of_var_decl vd
    | FuncDecl(fd) -> print_string_of_func_decl fd

let print_string_of_program (prog: program) =
    let nodes = List.map print_string_of_decl prog in
    let program = print_dot_node "Program" in
    List.iter (function num -> print_dot_edge program num) nodes
