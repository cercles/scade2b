{
(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == lexer_scade_error.mll                                                 == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

  open Lexing
  open Parser_scade_error

  exception Lexical_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let line_cmt = "--" [^'\n']* ['\n']
let sep = ['\t' '\r' ' ' ]+

let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let real = digit+ '.' digit+ exponent?
    | digit* '.' digit+ exponent?
    | digit+ exponent
let alpha = ['a'-'z''A'-'Z''_']
let ident = alpha (digit|alpha)*
let pragma_simpl = '#'(digit|alpha)+

  rule token = parse
    | sep                        { token lexbuf }
    | '\n'                       { newline lexbuf;
				   token lexbuf } 
    | line_cmt                   { newline lexbuf;
				   token lexbuf }
    | "/*"                       { comment lexbuf;
				   token lexbuf }
    | "#pragma"                  { pragma lexbuf;
				   token lexbuf }
    | '#' ((digit|alpha)+) { token lexbuf }

    | "node " (pragma_simpl)? ident
	{ node_text lexbuf;
	  NODE_DECL }
    | "function " (pragma_simpl)? ident  
        { node_text lexbuf;
	  NODE_DECL }

    | "tel"                      { TEL }
    | "assume"                   { ASSUME }
    | "guarantee"                { GUARANTEE }

    | "bool"                     { T_BOOL }
    | "int"                      { T_INT }
    | "real"                     { T_REAL }

    | "if"                       { IF }
    | "then"                     { THEN }
    | "else"                     { ELSE }

    | "fby"                      { FBY }

    | "Array_PRED"               { ARRAY_PRED }

    | '+'                        { PLUS }
    | '-'                        { MINUS }
    | '*'                        { MULT }
    | '/'                        { DIV }    
    | "div"                      { DIV_INT }
    | "mod"                      { MOD } 

    | '='                        { EQ }
    | "<>"                       { NEQ }
    | '<'                        { INF }
    | "<="                       { INFEQ }
    | '>'                        { SUP }
    | ">="                       { SUPEQ }

    | "and"                      { AND }
    | "or"                       { OR }
    | "not"                      { NOT }
    | "xor"                      { XOR }
    | '#'                        { SHARP }

    | "reverse"                  { REVERSE }
    | "transpose"                { TRANSPOSE }

    | '('                        { LPAREN }
    | ')'                        { RPAREN }
    | '['                        { LBRACKET }
    | ']'                        { RBRACKET }
    | ':'                        { COLON }
    | ';'                        { SEMICOL }
    | ','                        { COMMA }
    | ".."                       { DOTDOT }
    | '.'                        { DOT }
    | '^'                        { CARET }
    | '@'                        { CONCAT } 

    | "true"                     { BOOL (true) }
    | "false"                    { BOOL (false) }
    | digit+ as n                { INT (int_of_string n) }
    | real as r                  { REAL (float_of_string r) }
    | ident as id                { IDENT (id) }

    | eof                        { EOF }
    | _                          { raise (Lexical_error "lexical error") }

and node_text = parse
    | "let"                      { () }
    | sep                        { node_text lexbuf }
    | '\n'                       { newline lexbuf;
				   node_text lexbuf } 
    | line_cmt                   { newline lexbuf;
				   node_text lexbuf }
    | "/*"                       { comment lexbuf;
				   node_text lexbuf }
    | "#pragma"                  { pragma lexbuf;
				   node_text lexbuf }
    | '#' ((digit|alpha)+) { node_text lexbuf }
    | _                   { node_text lexbuf }

and eqs = parse
    | sep                        { eqs lexbuf }
    | '\n'                       { newline lexbuf;
				   eqs lexbuf } 
    | "/*"                       { comment lexbuf;
				   eqs lexbuf }
    | line_cmt                   { newline lexbuf;}
    | "#pragma"                  { pragma lexbuf;
				   eqs lexbuf }
    | '#' ((digit|alpha)+) { eqs lexbuf }


and pragma = parse
    | "#end" { () }
    | '\n'   { Lexing.new_line lexbuf; comment lexbuf }
    | _      { pragma lexbuf }
    | eof    { raise (Lexical_error "unterminated pragma") }


and comment = parse
    | "*/" { () }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _    { comment lexbuf }
    | eof  { raise (Lexical_error "unterminated comment") }


	{
	}
