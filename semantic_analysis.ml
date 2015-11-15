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

type symbol_table = {
    parent : symbol_table option;
    variables : variable_decleration list;
}

type environment = {
    return_type : flow_type option;
    scope : symbol_table;
}

let rec find_variable_type (scope : symbol_table) (name : string) : flow_type =
    try
        let vdecl = List.find (fun (var_decl) -> var_decl.declaration_id = name) scope.variables
        in vdecl.declaration_type
    with Not_found ->
        match scope.parent with
            Some(parent) -> find_variable_type parent name
          | _ -> raise Not_found

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
                find_variable_type env.scope s
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
