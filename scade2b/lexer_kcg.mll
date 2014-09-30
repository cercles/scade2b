{
(* =========================================================================== *)
(* == CERCLES2 -- ANR-10-SEGI-017                                           == *)
(* =========================================================================== *)
(* == lexer_kcg.mll                                                         == *)
(* ==                                                                       == *)
(* ==                                                                       == *)
(* =========================================================================== *)
(* == Florian Thibord - florian.thibord[at]gmail.com                        == *)
(* =========================================================================== *)

  open Lexing
  open Parser_kcg

  exception Lexical_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let buf = Buffer.create 500
  let count_let = ref 0

}

let line_cmt = "--" [^'\n']* ['\n']
let sep = ['\t' '\r' ' ']+

let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let real = digit+ '.' digit+ exponent?
  | digit* '.' digit+ exponent?
  | digit+ exponent
let alpha = ['a'-'z''A'-'Z''_']
let ident = alpha (digit|alpha)*

let pragma_simpl = '#'(digit|alpha)+

rule token = parse
          | sep                      { token lexbuf }
	  | '\n'                     { newline lexbuf;
				       token lexbuf } 
	  | line_cmt                 { newline lexbuf;
				       token lexbuf }
	  | "/*"                     { comment lexbuf;
				       token lexbuf }
	  | "#pragma"                { pragma lexbuf;
				       token lexbuf }
	  | pragma_simpl             { token lexbuf }

	  | "node " (pragma_simpl)? (ident as id)
	      { Buffer.reset buf;
		Buffer.add_string buf "node ";
		Buffer.add_string buf id;
		node_text lexbuf;
		NODE (id, (Buffer.contents buf)) }
	  | "function " (pragma_simpl)? (ident as id)  
              { Buffer.reset buf;
		Buffer.add_string buf "function ";
		Buffer.add_string buf id;
		node_text lexbuf;
		FUNCTION (id, (Buffer.contents buf)) }

	  | "package " ident         { token lexbuf }
	  | "package public " ident  { token lexbuf }
	  | "end;"                   { token lexbuf }

          | "type"                   { TYPE }
          | "enum"                   { ENUM }
          | "imported"               { IMPORTED }

	  | "const"                  { CONST }

	  | "bool"                   { T_BOOL }
	  | "int"                    { T_INT }
	  | "real"                   { T_REAL }

	  | '('                      { LPAREN }
	  | ')'                      { RPAREN }
	  | '['                      { LBRACKET }
	  | ']'                      { RBRACKET }
	  | '{'                      { LBRACE }
	  | '}'                      { RBRACE }
	  | ':'                      { COLON }
	  | ';'                      { SEMICOL }
	  | ','                      { COMMA }
	  | ".."                     { DOTDOT }
	  | '.'                      { DOT }
	  | '^'                      { CARET }
	  | '='                      { EQ }

	  | "true"                   { BOOL (true) }
	  | "false"                  { BOOL (false) }
	  | digit+ as n              { INT (int_of_string n) }
	  | real as r                { REAL (float_of_string r) }
	  | ident as id              { IDENT (id) }

	  | eof                      { EOF }
	  | _                        { token lexbuf }

and node_text = parse
    | "let"        { Buffer.add_string buf "let";
		     count_let := !count_let + 1;
		     node_text lexbuf }
    | "tel"        { Buffer.add_string buf "tel";
		     count_let := !count_let - 1;
		     if !count_let = 0 then () else node_text lexbuf }
    | '\n' as char { Lexing.new_line lexbuf; 
		     Buffer.add_char buf char;
		     node_text lexbuf }
    | "where"      { raise (Lexical_error "where clause") }
    | "specialize" { raise (Lexical_error "spec clause") }
    | "sig"        { raise (Lexical_error "sig clause") }
    | _ as char    { Buffer.add_char buf char;
		     node_text lexbuf }
	
and comment = parse
    | "*/" { () }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _    { comment lexbuf }
    | eof  { raise (Lexical_error "unterminated comment") }

and pragma = parse
    | "#end" { () }
    | '\n'   { Lexing.new_line lexbuf; comment lexbuf }
    | _      { pragma lexbuf }
    | eof    { raise (Lexical_error "unterminated pragma") }

and typ = parse
    | ";"    { () }
    | _      { typ lexbuf } 

{
}
