{
  (* Florian Thibord  --  Projet CERCLES *)

  open Lexing
  open Parser

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
          | sep  { token lexbuf }
	  | '\n' { newline lexbuf;
		   token lexbuf } 
	  | line_cmt { newline lexbuf;
		       token lexbuf }
	  | "/*"     { comment lexbuf;
		       token lexbuf }
	  | "#pragma"    { pragma lexbuf;
			   token lexbuf }
	  | pragma_simpl { token lexbuf }

	  | "node"|"function"  { NODE }
	  | "returns" { RETURNS }
	  | "let"     { LET }
	  | "tel"     { TEL }
	  | "var"     { VAR }
	  | "const"   { CONST }
	  | "assume"  { ASSUME }
	  | "guarantee" { GUARANTEE }
	  (* | "include" { INCLUDE } *)
	  (* | "assert"  { ASSERT } *)

	  | "bool" { T_BOOL }
	  | "int"  { T_INT }
	  | "real" { T_REAL }

	  | "if"   { IF }
	  | "then" { THEN }
	  | "else" { ELSE }

	  (* | "pre" { PRE } *)
	  (* | "->"  { ARROW } *)
	  | "fby" { FBY }

	  | '+'   { PLUS }
	  | '-'   { MINUS }
	  | '*'   { MULT }
	  | '/'   { DIV }    
	  | "div" { DIV_INT }
	  | "mod" { MOD } 

	  | '='  { EQ }
	  | "<>" { NEQ }
	  | '<'  { INF }
	  | "<=" { INFEQ }
	  | '>'  { SUP }
	  | ">=" { SUPEQ }

	  | "and" { AND }
	  | "or"  { OR }
	  | "not" { NOT }
	  | "xor" { XOR }
	  | '#'   { SHARP }

	  | '('  { LPAREN }
	  | ')'  { RPAREN }
	  | '['  { LBRACKET }
	  | ']'  { RBRACKET }
	  | ':'  { COLON }
	  | ';'  { SEMICOL }
	  | ','  { COMMA }
	  | '"'  { QUOTES }
	  | ".." { DOTDOT }
	  | '.'  { DOT }
	  | '^'  { CARET }
	  | '|'  { CONCAT } 

	  | "true"      { BOOL (true) }
	  | "false"     { BOOL (false) }
	  | digit+ as n { INT (int_of_string n) }
	  | real as r   { REAL (float_of_string r) }
	  | ident as id { IDENT (id) }

	  | eof { EOF }
	  | _   { raise (Lexical_error "lexical error") }

and comment = parse
    | "*/" { () }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _    { comment lexbuf }
    | eof  { raise (Lexical_error "unterminated comment") }

and pragma = parse
    | "#end" { () }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _      { pragma lexbuf }
    | eof    { raise (Lexical_error "unterminated pragma") }


{
}
