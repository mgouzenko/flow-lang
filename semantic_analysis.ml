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
    else raise (Error(decl.declaration_id ^ ": Declaration type does not match expression"))

and check_function_declaration (env: environment) (decl: function_declaration) =
    "hello"

and check_struct_declaration (env: environment) (decl: struct_declaration) =
    "hello"

let check_declaration (env: environment) (decl: s_declaration) -> (environment, s_declaration) =
    match decl with
      VarDecl(vdecl) -> check_variable_declaration env vdecl
    | FuncDecl(fdecl) -> check_function_declaration env fdecl
    | StructDecl(sdecl) -> check_struct_declaration env sdecl

let check_progam (prog: program) =
    let env = {
        return_type = None;
        scope = { parent : None; variables : []; };
        funcs : [];
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
