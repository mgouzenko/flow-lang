open Ast
  
type s_dot_initializer =
  { s_dot_initializer_id : string; s_dot_initializer_val : typed_expr
  }
  and (* typed expression *)
  expr_details =
  | TIntLiteral of int
  | TStringLiteral of string
  | TBoolLiteral of bool
  | TCharLiteral of char
  | TDoubleLiteral of float
  | TListInitializer of typed_expr list
  | TId of string
  | TBinOp of typed_expr * bin_op * typed_expr
  | TUnaryOp of unary_op * typed_expr
  | TFunctionCall of string * typed_expr list
  | TNoexpr
  and typed_expr =
  (expr_details * flow_type)

type s_variable_declaration =
  { s_declaration_type : flow_type; s_declaration_id : string;
    s_declaration_initializer : typed_expr
  }

type s_stmt =
  | SExpr of typed_expr
  | SBlock of s_stmt list
  | SReturn of typed_expr
  | SDeclaration of s_variable_declaration
  | SIf of typed_expr * s_stmt * s_stmt
  | SFor of typed_expr * typed_expr * typed_expr * s_stmt
  | SWhile of typed_expr * s_stmt
  | SContinue
  | SBreak
  | SPoison of typed_expr
  | SExitProc

type s_function_declaration =
  { s_return_type : flow_type; s_function_name : string;
    s_arguments : s_variable_declaration list; s_has_definition : bool;
    s_body : s_stmt list
  }

type s_declaration =
  | SVarDecl of s_variable_declaration | SFuncDecl of s_function_declaration

type s_program = s_declaration list


