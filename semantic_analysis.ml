open Ast;;
open Sast;;

(* helper functions, TODO: need to be modularized, some used in printer.ml *) 


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

let rec string_of_op = function
      Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Modulo -> "%"
    | Neq -> "!="
    | Lt -> "<"
    | Leq -> "<="
    | Gt -> ">"
    | Geq -> ">="
    | Eq -> "=="
    | Send -> "->"
    | And -> "&&"
    | Or -> "||"
    | Assign -> "="
    | Retrieve -> "@"
    | Negate -> "-"
    | Not -> "!"
    | Wait -> "^"

(* translate flow ast to s_ast *)

type symtab = {
    parent : symtab option;
    variables : variable_declaration list;
}

type function_entry = {
    name: string;
    param_types: flow_type list;
    ret_type: flow_type;
}

type environment = {
    return_type : flow_type option;
    symbol_table: symtab;
    funcs : function_entry list;
    in_loop: bool;
}

let rec find_variable_decl (symbol_table: symtab) (name : string) : flow_type =
    try
        List.find (fun (var_decl) -> var_decl.declaration_id = name) symbol_table.variables
    with Not_found ->
        match symbol_table.parent with
            Some(parent) -> find_variable_type parent name
          | _ -> raise Not_found

let find_variable_type (symbol_table: symtab) (name : string) : flow_type =
    let vdecl = find_variable_decl symbol_table name in
    vdecl.declaration_type

let type_of_texpr = function
      TIntLiteral(i) -> Int
    | TStringLiteral(s) -> String
    | TBoolLiteral(b) -> Bool
    | TCharLiteral(c) -> Char
    | TDoubleLiteral(d) -> Double
    | TStructInitializer(_, t) -> t
    | TArrayInitializer(_, t) -> t
    | TArrayElement(_, _, t) -> t
    | TId(_, t) -> t
    | TBinOp(_, _, _, t) -> t
    | TUnaryOp(_, _, t) -> t
    | TAssign(_, _, t) -> t
    | TFunctionCall(_, _, t) -> t
    | TNoexpr -> raise (Error("Type of expression called on Noexpr"))

let check_binop (e1 : texpr) (e2 : texpr) (op : bin_op) : texpr =
    let t1 = type_of_texpr e1
    and t2 = type_of_texpr e2
    in
    match (op) with
      (Plus | Minus | Times | Divide | Modulo | Lt | Leq | Gt | Geq) ->
         (match (t1, t2) with
          | (Int, Int) -> TBinop(e1, op, e2, Int)
          | (_, _) -> raise (Error("operator " ^ string_of_binop op ^ " not"
            ^ " compatible with " ^ string_of_type t1 ^ " and " ^ string_of_type t2))
    | (_) -> raise (Error("finish checking binops"))

let rec check_expr (env : environment) (e : expr) : texpr =
    match e with
      IntLiteral(i) -> TIntLiteral(i)
    | StringLiteral(s) -> TStringLiteral(s)
    | BoolLiteral(b) -> TBoolLiteral(b)
    | CharLiteral(c) -> TCharLiteral(c)
    | DoubleLiteral(d) -> TDoubleLiteral(d)
    | Id(s) ->
        let t = 
            try
                find_variable_type env.symbol_table s
            with Not_found -> raise (Error("Unrecognized identifier " ^ s))
        in TId(s, t)
    | BinOp(e1, op, e2) ->
        let checked_e1 = check_expr env e1
        and checked_e2 = check_expr env e2
        in check_binop checked_e1 checked_e2 op
    (*
    | TStructInitializer of dot_initializer list * flow_type
    | TArrayInitializer of texpr list * flow_type
    | TArrayElement of string * texpr * flow_type
    | TUnaryOp of unary_op * texpr * flow_type
    | TAssign of string * texpr * flow_type
    | TFunctionCall of string * texpr list * flow_type
    | TNoexpr
    *)

let check_variable_declaration (env: environment) (decl: variable_declaration) =
    let expr_details, t = check_expr env decl.declaration_initializer in
    if t = decl.declaration_type then
        (try let _ =
            (* Try to find the a local variable of the same name. If found, it's an error. *)
            List.find
            (fun (vdecl) -> vdecl.declaration_id = decl.declaration_id)
            env.symbol_table.variables in
        raise (Error("Variable " ^ decl.declaration_id ^ " already declared in local scope"))

        (* If not found, add the declaration to the symbol table and return the new environment *)
        with Not_found ->
            let new_symbol_table = {env.symbol_table with variables = decl::symbol_table.variables } in
            let new_env =  { env with symbol_table = new_symbol_table }
            and s_var_decl = {
                s_declaration_type = decl.declaration_type;
                s_declaration_id = decl.declaration_id;
                s_declaration_initializer = (expr_details, t) } in
            (new_env, s_var_decl))
    else raise (Error(decl.declaration_id ^ ": Declaration type does not match expression")) in

let check_arg_declaration (env: environment) (decl: variable_declaration) =
    match decl.declaration_initializer with
        (* Todo: make sure channels have correct directions *)
        Noexpr -> check_variable_declaration env decl
        | _ -> raise (Error("Error in argument declaration for " ^
                            decl.declaration_id ^
                            ": Cannot have default values in function declaration.")) in

let is_logical (expr: typed_expr) = match expr with
      _, Int | _, Bool | _, Channel(t, dir) | _, Char | _, String -> true
    | _ -> false

let rec check_stmt (env: environment) (stmt: stmt) = match stmt with
      Expr(e) -> (env, SExper(check_expr env e))
    | Block(stmt_list) ->
            let _, checked_stmts = check_stmt_list env stmt_list in
            (env, SBlock(checked_stmts))
    | Return(e) ->
            let expr_details, t = check_expr env e in
            if t = env.return_type then (env, SReturn(check_expr env e))
            else raise(Error("Expression does not match return_type"))
    | Declaration(vdecl) -> check_variable_declaration env vdecl
    | If(e, s1, s2) ->
            let checked_expr = check_expr env e
            and _, checked_stmt1 = check_stmt env s1
            and _, checked_stmt2 = check_stmt env s2 in
            if is_logical checked_expr then
                (env, SIf(checked_expr, checked_stmt1, checked_stmt2))
            else raise(Error("Invalid expression in \"if\" statement"))
    | For(e1, e2, e3, s) ->
            let checked_expr1 = check_expr env e1
            and checked_expr2 = check_expr env e2
            and checked_expr3 = check_expr env e3
            and _, checked_stmt = check_stmt {env with in_loop = true} s in
            if is_logical checked_expr1 &&
               is_logical checked_expr2 &&
               is_logical checked_expr3 then
                   (env, SFor(checked_expr1,
                             checked_expr2,
                             checked_expr3,
                             checked_stmt))
            else raise(Error("Invalid expression in \"for\" statement"))
    | While(e, s) ->
            let checked_expr = check_expr env e
            and _, checked_stmt = check_stmt {env with in_loop = true} s in
            if is_logical checked_expr then
                SWhile(checked_expr, checked_stmt)
            else raise(Error("Invalid expression in \"while\" statement"))
    | Continue ->
            if env.in_loop = true then (env, SContinue)
            else raise(Error("Not in a loop")
    | Break ->
            if env.in_loop = true then (env, SBreak)
            else raise(Error("Not in a loop")
    | Poison(e) ->
            let expr_details, t = check_expr e in
            match t with
                  Channel(t, dir) -> (env, SPoison(expr_details, t))
                | _ -> raise(Error("Attempting to poison a non-channel"))

and rec check_stmt_list (env: environment) (stmt_list: stmt list) =
    let new_env, checked_stmts = List.fold_left
    (fun acc stmt ->
        let env', stmt_node = check_stmt (fst acc) stmt in
        (env', stmt_node::(snd acc)))
    (env, []) fdecl.arguments in
    (new_env, List.rev checked_stmts)

let check_function_declaration (env: environment) (fdecl: function_declaration) =
    (* Get the types of the function's parameters *)
    let p_types = List.map (fun vdecl -> vdecl.declaration_type) fdecl.arguments in

    let f_entry = { name = fdecl.function_name;
                    param_types: p_types;
                    ret_type: flow_type;} in

    let new_funcs = f_entry::env.funcs in

    (* Make a new symbol table for the function scope *)
    let new_symbol_table = { parent = env.symbol_table;
                             variables = [] } in

    (* Add the function currently being checked to the environment. This is
     * needed in the case of recursion (ie encountering a function call
     * referencing this function in the body). Furthermore, set the return
     * type and symbol table with now-empty local scope *)
    let new_env = { env with funcs = new_funcs;
                             return_type = Some(fdecl.return_type);
                             symbol_table = new_symbol_table } in

    (* Get the arguments into the scope by folding the environment
     * over the parameter list *)
    let env_with_args, arg_decl_list = List.fold_left
    (fun acc arg_decl ->
        let env', arg_node = check_arg_declaration (fst acc) arg_decl in
        (env', arg_node::(snd acc)))
    (new_env, []) fdecl.arguments in

    (* Check the function body. Discard the environment. We won't need it
     * outside the scope of the function body. *)
    let _, func_body = check_stmt_list env_with_args fdecl.body in

    (* Create the function node to return *)
    let func_node = { s_return_type = fdecl.return_type;
      s_function_name = fdecl.function_name;
      s_arguments = arg_decl_list;
      s_has_definition = true;
      s_body = func_body; } in

    (* Return the original environment, with the current function appended *)
    ({ env with funcs = new_funcs }, func_node) in

let check_struct_declaration (env: environment) (decl: struct_declaration) =
    "hello" in

let check_declaration (env: environment) (decl: s_declaration) -> (environment, s_declaration) =
    match decl with
      VarDecl(vdecl) -> check_variable_declaration env vdecl
    | FuncDecl(fdecl) -> check_function_declaration env fdecl
    | StructDecl(sdecl) -> check_struct_declaration env sdecl

let check_progam (prog: program) =
    let env = {
        return_type = None;
        symbol_table = { parent : None; variables : []; };
        funcs = [];
        in_loop = false;
    } in

    (* acc is the accumulator; it's a tuple of env, decl_list.
     * the fold left builds the accumulator, threading the environment
     * through the list of declarations. When the fold finishes, decl_list
     * should be a built list of s_declarations *)
    let _, decl_list = List.fold_left
    (fun acc decl ->
        let new_env, snode = check_declaration (fst acc) decl in
        (new_env, snode::(snd acc)))
    (env, []) program in decl_list
