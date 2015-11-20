open Ast;;
open Sast;;

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
          in print_dot_node opstring;;

let print_string_of_unop op =
    let opstring = match op with
          Retrieve -> "@"
        | Wait -> "^"
        | Negate   -> "-"
        | Not      -> "!" in
    print_dot_node opstring

let rec print_string_of_expr = function
    TIntLiteral(l), _ -> print_dot_node(string_of_int l)
  | TStringLiteral(s), _ -> print_dot_node(s)
  | TBoolLiteral(b), _ -> print_dot_node(string_of_bool b)
  | TCharLiteral(c), _ -> print_dot_node (Char.escaped c)
  | TDoubleLiteral(d), _ -> print_dot_node "Some double"
  | TStructInitializer(decl_list), _ -> print_dot_node "StructInitializer"
  | TArrayInitializer(expr_list), _ -> 
          let arrinitnode = print_dot_node "ArrayInitializer"
          in let enodes = List.map print_string_of_expr expr_list
          in let _ = List.iter (function enode -> print_dot_edge arrinitnode enode) 
                                enodes 
          in arrinitnode
  | TArrayElement(name, idx), _ -> 
          let arrelemnode = print_dot_node name
          in let idxnode = print_string_of_expr idx
          in let _ = print_dot_edge arrelemnode idxnode ~desc: "index"
          in arrelemnode 
  | TId(s), _ -> print_dot_node s
  | TBinOp(e1, op, e2), _ ->
          let num1 = print_string_of_expr e1
          and num2 = print_string_of_expr e2
          and opnum = print_string_of_binop op in
          let _ = print_dot_edge opnum num1
          and _ = print_dot_edge opnum num2  in
          opnum
  | TUnaryOp(op, e), _ -> let num = print_string_of_expr e
                      and op = print_string_of_unop op
                      in let _ = print_dot_edge op num in op
  | TFunctionCall(f, el), _ ->
          let f_node = print_dot_node f in
          let enodes = List.map print_string_of_expr el in
          let _ = List.iter (function num -> print_dot_edge f_node num) enodes
          in f_node
  | TNoexpr, _ -> print_dot_node "Noexpr"


let rec string_of_type = function
      Int ->  "int"
    | Float -> "float"
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
    | Array(t, n, s) -> s ^ "<" ^ string_of_type t ^ ">[" ^ string_of_int n ^ "]"
    | Struct(s) -> s;;

let print_string_of_type t =
    print_dot_node (string_of_type t)

let print_string_of_var_decl vdecl =
    let decl_node = print_dot_node "svdecl"
    and type_node = print_string_of_type vdecl.s_declaration_type
    and name_node = print_dot_node vdecl.s_declaration_id
    and expr_node = print_string_of_expr vdecl.s_declaration_initializer
    in let _ = print_dot_edge decl_node type_node ~desc: "type"
    and _ = print_dot_edge decl_node name_node  ~desc: "name"
    and _ = print_dot_edge decl_node expr_node ~desc: "initializer"
    in decl_node

let print_string_of_struct_decl sdecl =
    let struct_node = print_dot_node "struct"
    and name_node = print_dot_node sdecl.s_struct_name
    and members_node = print_dot_node "members" in
    let _ = print_dot_edge struct_node name_node ~desc: "name"
    and _ = print_dot_edge struct_node members_node
    and _ = List.iter (fun vdecl ->
        let vdecl_node = print_string_of_var_decl vdecl in
        print_dot_edge members_node vdecl_node) sdecl.s_struct_members in
    struct_node

let rec print_string_of_stmt = function
      SExpr(e) -> print_string_of_expr e
    | SBlock(stmt_list) ->
            let block_node = print_dot_node "block" in
            let _ = List.iter (fun stmt ->
                let stmt_node = print_string_of_stmt stmt in
                print_dot_edge block_node stmt_node) stmt_list in
            block_node
    | SReturn(e) -> print_string_of_expr e
    | SDeclaration(vdecl) -> print_string_of_var_decl vdecl
    | SIf(e, s1, s2) ->
            let ifnode = print_dot_node "if"
            and enode  = print_string_of_expr e
            and snode1 = print_string_of_stmt s1
            and snode2 = print_string_of_stmt s2 in
            let _ = print_dot_edge ifnode enode ~desc: "cond"
            and _ = print_dot_edge ifnode snode1 ~desc: "true"
            and _ = print_dot_edge ifnode snode2 ~desc: "false" in
            ifnode
    | SFor(e1, e2, e3, s) ->
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
    | SWhile(e, s) ->
            let while_node = print_dot_node "while"
            and enode = print_string_of_expr e
            and snode = print_string_of_stmt s in
            let _ = print_dot_edge while_node enode ~desc: "cond"
            and _ = print_dot_edge while_node snode ~desc: "body"
            in while_node
    | SContinue -> print_dot_node "continue"
    | SBreak -> print_dot_node "break"
    | SPoison(chan_id) -> 
            let pnode = print_dot_node "poison"
            and inode = print_string_of_expr chan_id in 
            let _ = print_dot_edge pnode inode
            in pnode

let print_string_of_func_decl fdecl =
    let fdecl_node = print_dot_node "fdecl"
    and type_node = print_string_of_type fdecl.s_return_type
    and name = print_dot_node fdecl.s_function_name
    and args_node = print_dot_node "args"
    and args = List.map print_string_of_var_decl fdecl.s_arguments
    and body = print_dot_node "body" in
    let _ = print_dot_edge fdecl_node type_node ~desc: "type"
    and _ = print_dot_edge fdecl_node name ~desc: "name"
    and _ = print_dot_edge fdecl_node args_node
    and _ = List.iter (fun node -> print_dot_edge args_node node) args
    and _ = print_dot_edge fdecl_node body
    and _ = List.iter (fun stmt ->
                let stmt_node = print_string_of_stmt stmt in
                print_dot_edge body stmt_node) fdecl.s_body in
    fdecl_node

let print_string_of_decl = function
      SVarDecl(vd) -> print_string_of_var_decl vd
    | SFuncDecl(fd) -> print_string_of_func_decl fd
    | SStructDecl(sd) -> print_string_of_struct_decl sd

let print_string_of_program (prog: s_program) =
    let nodes = List.map print_string_of_decl prog in
    let program = print_dot_node "Program" in
    List.iter (function num -> print_dot_edge program num) nodes
