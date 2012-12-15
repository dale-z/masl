(* Scanner for MASL. *)

(* Header section. *)
{
	open Parser
	(* TODO Keep track of character position. *)
	let inc_lnum lexbuf =
		let pos = lexbuf.Lexing.lex_curr_p in
		lexbuf.Lexing.lex_curr_p <- {pos with
			Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
			Lexing.pos_bol = pos.Lexing.pos_cnum;
		}
	;;

	let explode s =
		let rec exp i l =
			if i < 0 then l else exp (i - 1) (s.[i]::l) in
		exp (String.length s - 1) [];;
}

(* Definition section *)

let whitespace = [' ' '\t' '\r' '\n']
let nonwhitespace = _#whitespace
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let input_char = _#['\r' '\n']
let common_escape_sequence = "\\t" | "\\r" | "\\n" | "\\0"
let single_char = input_char#['\'' '\\']
let escape_sequence_char = common_escape_sequence | "\\'"
let single_char_string = input_char#['"' '\\']
let escape_sequence_string = common_escape_sequence | "\\\""

(* Rule section. *)

rule single_comment_parser = parse
	'\n'	{ inc_lnum lexbuf; token_parser lexbuf }
	| _#['\n']	{ single_comment_parser lexbuf }

and multiline_comment_parser = parse
	"*/"	{ token_parser lexbuf }
	| _	as char { if char == '\n' then inc_lnum lexbuf; multiline_comment_parser lexbuf }

and token_parser = parse
	(* Whitespaces. *)
	whitespace as char	{ if char == '\n' then inc_lnum lexbuf; token_parser lexbuf }
	(* Comments. *)
	| "//"	{ single_comment_parser lexbuf }
	| "/*"	{ multiline_comment_parser lexbuf }
	(* Separators. *)
	| ':' { COLON }
	| ';'	{ SEMICOLON }
	| ','	{ COMMA }
	| '('	{ LPAREN }
	| ')'	{ RPAREN }
	| '{'	{ LBRACE }
	| '}'	{ RBRACE }
	| '['	{ LSQBRA }
	| ']'	{ RSQBRA }
	(* Keywords. *)
	| "int"	{ INT }
	| "double"	{  DOUBLE }
	| "char"	{ CHAR }
	| "bool"	{ BOOL }
	| "class"	{ CLASS }
	| "object"	{ OBJECT }
	| "fun" { FUN }
	| "void"	{ VOID }
	| "if"	{ IF }
	| "else"	{ ELSE }
	| "for"	{ FOR }
	| "while"	{ WHILE }
	| "do"	{ DO }
	| "continue"	{ CONTINUE }
	| "break"	{ BREAK }
	| "return"	{ RETURN }
	| "this"	{ THIS }
	| "state"	{ STATE }
	(* Operators. *)
	| '+'	{ PLUS }
	| '-'	{ MINUS }
	| '*'	{ MULT }
	| '/'	{ DIV }
	| '%'	{ MOD }
	| "&&"	{ AND }
	| "||"	{ OR }
	| '!'	{ NOT }
	| '>'	{ GT }
	| ">="	{ GE }
	| "=="	{ EQ }
	| "!="	{ NEQ }
	| "<="	{ LE }
	| '<'	{ LT }
	| '='	{ ASSIGN }
	| '.'	{ DOT }
	| '@'	{ AT }
	| "->"	{ TRANS }
	(* Identifiers. *)
	| (letter | '_') (letter | '_' | digit)* as lxm { ID(lxm) }
	(* Literals (for basic data types). *)
	| digit+ as lxm	{ INT_LITERAL(int_of_string lxm) }
	| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)
	| digit+ ('.' digit*) (['e' 'E'] ['+' '-']? digit+)?
	| digit* '.' digit+ (['e' 'E'] ['+' '-']? digit+)?	as lxm { DOUBLE_LITERAL(float_of_string lxm) }
	| '\'' single_char '\'' as lxm	{ CHAR_LITERAL(lxm.[1]) }
	| '\''  escape_sequence_char '\'' as lxm { CHAR_LITERAL(Scanf.sscanf ("\"" ^ lxm ^ "\"") "%S%!" (fun u -> u.[1])) }
	| "true" | "false" as lxm	{ BOOL_LITERAL(bool_of_string lxm) }
	| '\"' (single_char_string | escape_sequence_string)* '\"'	as lxm { STRING_LITERAL(explode (Scanf.sscanf ("\"" ^ lxm ^ "\"") "%S%!" (fun u -> u))) }
	| _ as lxm { raise (Failure("illegal token" ^ (Char.escaped lxm))); }
	| eof { EOF }
	(* TODO Handle EOF and invalid input. *)

(* Trailer section. *)
{
}