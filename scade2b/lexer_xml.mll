{
  (* Florian Thibord  --  Projet CERCLES *)

  open Lexing
  open Parser_xml

  exception Lexical_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let buf = Buffer.create 500
  let count_let = ref 0
}

let sep = ['\t' '\r' ' ']+

let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let real = digit+ '.' digit+ exponent?
  | digit* '.' digit+ exponent?
  | digit+ exponent
let alpha = ['a'-'z''A'-'Z''_']
let ident = (digit|alpha)+

(* let unused = "OutCtxVar" | "OutCtxType" | "InCtxVar" | "InCtxType" | "Constant" | "Init" | "Package" | "Option"  | "StructType" | "Field" | "EnumType" | "EnumVal" | "NamedType" | "ExpNode" | "Iterator" | "NodeInlining" | "Memory" | "Automaton" | "State" | "Fork" | "Condition" | "Transition" | "ActiveState" | "NextState" | "SelectedState" | "Clock" | "ResetActiveState" | "ResetNextState" *)


(* "Input" | "Output" | "Local" | "PredefType" | "ArrayType" *)


rule token = parse
          | sep              { token lexbuf }
	  | '\n'             { newline lexbuf;
			       token lexbuf }
	  | "<!--"           { comment lexbuf;
			       token lexbuf }
	  | "<?" [^'>']* '>' { token lexbuf }

	  | "Model"          { MODEL }
	  | "RootNode"       { ROOTNODE }
	  | "NoExpNode"      { NOEXPNODE }
	  | "NodeInstance"   { NODEINSTANCE }
	  | "Input"          { INPUT }
	  | "Output"         { OUTPUT }
	  | "ArrayType"      { ARRAYTYPE }
	  (* | "PredefType"     { PREDEFTYPE } *)

	  | "scadeName"      { SCADENAME }
	  | "instName"       { INSTNAME }
	  | "targetType"     { TARGETTYPE }
	  | "targetName"     { TARGETNAME }
	  | "cellType"       { CELLTYPE }
	  | "size"           { SIZE }

	  | '='              { EQ }
	  | '/'              { SLASH }
	  | '<'              { CHEV_IN }
	  | '>'              { CHEV_OUT }

	  (* | unused           { UNUSED } *)
	  | ident as id      { IDENT (id) }

	  | '"'              { Buffer.reset buf;
			       option_value lexbuf;
			       VALUE (Buffer.contents buf) }

	  | eof              { EOF }
	  | _                { raise (Lexical_error "Anomaly in kcg_traces.xml") }

and comment = parse
    | "-->" { () }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _    { comment lexbuf }
    | eof  { raise (Lexical_error "unterminated comment") }

and option_value = parse
  | '"'        { () }
  | _ as char  { Buffer.add_char buf char;
		 option_value lexbuf }


{
}
