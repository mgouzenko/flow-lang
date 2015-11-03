type bin_op =
        Plus | Minus | Times | Divide | Modulo
      | Neq  | Lt    | Leq   | Gt     | Geq    | Eq
      | Send | And   | Or    | Assign

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
  | FunctionCall of string * expr list
  | Noexpr

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
    function_name: string;
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
