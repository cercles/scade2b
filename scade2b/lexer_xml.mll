{
  (* Florian Thibord  --  Projet CERCLES *)

  open Lexing
  open Parser_xml

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
let ident = (digit|alpha) (digit|alpha|['-'':''/''\\''.'' '])*

let option_list = (' '? ident '=''"' ident '"')+

rule token = parse
          | sep              { token lexbuf }
	  | '\n'             { newline lexbuf;
			       token lexbuf } 
	  | line_cmt         { newline lexbuf;
			       token lexbuf }
	  | "<!--"           { comment lexbuf;
			       token lexbuf }
	  | "<?" [^'>']* '>' { token lexbuf }

	  | (ident as op) '=''"' (ident as v) '"'  { OPTION(op, v) }

	  | "NoExpNode"      { NOEXPNODE }
	  | "NodeInstance"   { NODEINSTANCE }
	  | "RootNode"       { ROOTNODE }
	  | "scadeName"      { SCADENAME }
	  | "Model"          { MODEL }

	  | '"'              { QUOTES }
	  | '='              { EQ }
	  | '/'              { SLASH }
	  | '<'              { CHEV_IN }
	  | '>'              { CHEV_OUT }
	  
	  | ident as id { IDENT (id) }

	  | eof { EOF }
	  | _ { raise (Lexical_error "Anomaly in kcg_traces.xml") }

and comment = parse
    | "-->" { () }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _    { comment lexbuf }
    | eof  { raise (Lexical_error "unterminated comment") }

{
}
