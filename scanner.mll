{ open Parser }

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
| "<<"          { SHIFT_LEFT }
| ">>"          { SHIFT_RIGHT }
| '~'           { BIT_NOT}
| '!'           { NEGATE }
| '|'           { BIT_OR }
| '&'           { BIT_AND }
| '^'           { BIT_XOR }
| "||"          { OR }
| "&&"          { AND }
| '%'           { MODULO }
| '@'           { READ_CHANNEL }
| "->"          { WRITE_CHANNEL }
| "if"          { IF }
| "else"        { ELSE }
| "for"         { FOR }
| "while"       { WHILE }
| "return"      { RETURN }
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
| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| ['_' 'a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
