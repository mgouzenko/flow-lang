type bin_op =
        Plus | Minus | Times | Divide | Modulo
    | Equal | Neq | Lt | Leq | Gt | Geq | Eq
    | Send  | And | Or | Assign

type unary_op = Retrieve | Negate | Not
type direction = In | Out | Nodir

(* All of the primitive and nonprimitive types *)
type flow_type =
      Int
    | Float
    | Double
    | Bool
    | Char
    | Void
    | Proc
    | String
    | Channel of flow_type * direction
    | Struct of string    (* Needs a string representing struct type *)

type dot_initializer = {
    dot_initializer_id: string;
    dot_initializer_val: expr;
}

and expr =
    IntLiteral of int
  | StringLiteral of string
  | BoolLiteral of bool
  | CharLiteral of char
  | DoubleLiteral of float
  | StructInitializer of dot_initializer list
  | Id of string
  | BinOp of expr * bin_op * expr
  | UnaryOp of unary_op * expr
  | Assign of string * expr
  | Call of string * expr list
  | FunctionCall of expr list
  | Noexpr
  | Todo

type variable_declaration = {
    declaration_type: flow_type;
    declaration_id: string;
    declaration_initializer: expr;
}

type stmt =
    Expr of expr
  | Block of stmt list
  | Return of expr
  | Declaration of variable_declaration
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Continue
  | Break

type function_declaration = {
    return_type: flow_type;
    process_name: string;
    arguments: variable_declaration list;
    body : stmt list;
}

type struct_declaration = {
    struct_name: string;
    struct_members: variable_declaration list;
}

type declaration =
      VarDecl of variable_declaration
    | FuncDecl of function_declaration
    | StructDecl of struct_declaration

type program = Declarations of declaration list



(*string list * func_decl list*)

(*
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)*)
