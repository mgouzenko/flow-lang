open Ast;;

(* semantically analyzed ast *)

(* typed expression *)
type texpr = expr * flow_type
(*
    TIntLiteral of int
  | TStringLiteral of string
  | TBoolLiteral of bool
  | TCharLiteral of char
  | TDoubleLiteral of float
  | TStructInitializer of dot_initializer list * flow_type
  | TArrayInitializer of texpr list * flow_type
  | TArrayElement of string * texpr * flow_type
  | TId of string * flow_type
  | TBinOp of texpr * bin_op * texpr * flow_type
  | TUnaryOp of unary_op * texpr * flow_type
  | TAssign of string * texpr * flow_type
  | TFunctionCall of string * texpr list * flow_type
  | TNoexpr
  *)

(* typed statements *)
(*
type tstmt =
    TExpr of texpr * flow_type
  | TBlock of tstmt list
  | TReturn of texpr * flow_type
  | TDeclaration of variable_declaration
  | TIf of texpr * tstmt * tstmt
  | TFor of texpr * texpr * texpr * tstmt
  | TWhile of texpr * tstmt
  | TContinue
  | TBreak
  | TPoison of texpr
  *)
