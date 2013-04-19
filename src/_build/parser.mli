type token =
  | NODE
  | RETURNS
  | LET
  | TEL
  | VAR
  | CONST
  | ASSERT
  | IF
  | THEN
  | ELSE
  | PRE
  | FBY
  | PLUS
  | MINUS
  | MULT
  | DIV
  | DIV_INT
  | MOD
  | EQ
  | NEQ
  | INF
  | INFEQ
  | SUP
  | SUPEQ
  | AND
  | OR
  | NOT
  | XOR
  | SHARP
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COLON
  | SEMICOL
  | COMMA
  | DOTDOT
  | CARET
  | CONCAT
  | T_BOOL
  | T_INT
  | T_REAL
  | BOOL of (bool)
  | INT of (int)
  | REAL of (float)
  | IDENT of (string)
  | EOF

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast_repr.prog
