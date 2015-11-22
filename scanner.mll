{ open Parser }

(* Definitions *)

let digit = ['0'-'9']
let double = ((digit+ '.' digit*) | ('.' digit+))  

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '#'           { comment lexbuf }           (* Comments *)
| '.'           { DOT }
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| '['           { LBRACKET}
| ']'           { RBRACKET}
| ';'           { SEMI }
| ','           { COMMA }
| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { TIMES }
| '/'           { DIVIDE }
| '='           { ASSIGN }
| "=="          { EQ }
| "!="          { NEQ }
| '<'           { LT }
| "<="          { LEQ }
| ">"           { GT }
| ">="          { GEQ }
| '!'           { NOT }
| "||"          { OR }
| "&&"          { AND }
| '%'           { MODULO }
| '@'           { READ_CHANNEL }
| '^'           { WAIT_FOR_MORE } (* Temporary *)
| "->"          { WRITE_CHANNEL }
| "if"          { IF }
| "else"        { ELSE }
| "for"         { FOR }
| "while"       { WHILE }
| "return"      { RETURN }
| "poison"		  { POISON }
| "int"         { INT }
| "double"      { DOUBLE }
| "char"        { CHAR }
| "bool"        { BOOL }
| "break"       { BREAK }
| "continue"    { CONTINUE }
| "string"      { STRING }
| "list"        { LIST }
| "in"          { IN }
| "out"         { OUT }
| "channel"     { CHANNEL }
| "proc"        { PROC }
| "void"        { VOID }
| "struct"      { STRUCT }
| "true"        { BOOL_LITERAL(true) }
| "false"       { BOOL_LITERAL(false) }
| digit+ as lxm { INT_LITERAL(int_of_string lxm) }
| double as lxm { DOUBLE_LITERAL(float_of_string lxm)}
| '\"' ([^'\"']* as lxm) '\"' { STRING_LITERAL(lxm) }
| '\'' ([' '-'&' '('-'[' ']'-'~'] as lxm) '\'' { CHAR_LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { IDENTIFIER(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
