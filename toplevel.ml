type action = Ast | Translate | Compile | Version

let print_ast ast = Astutils.print_program ast;;
let translate_ast translate_name ast = Translate.translate translate_name ast;;
let compile_ast ast = ();;
let print_version () = 
	print_string (
		"\n**** Multi-Agent Simulation Language Compiler v 0.0.1 **** \n\n" ^
		"Usage:\n\n" ^
		"  masl -a | -t | -c | -v source_file\n\n" ^
		"  -a               Print the AST of a program.\n\n" ^
		"  -t               Translate a MASL source program into a Java\n" ^
		"                   source file (.java).\n\n" ^
		"  -c               Compile a MASL source program into a Java\n" ^
		"                   class (.java).\n\n" ^
		"  -v               Display the version and usage of this program.\n\n" ^
		"  source_file      The MASL source file.\n\n");;

print_string "\n";;

let main =
	let action =
		try
			if Array.length Sys.argv > 1 then
	  		List.assoc
	  			Sys.argv.(1)
	  			[("-a", Ast); ("-t", Translate); ("-c", Compile); ("-v", Version)]
			else Version 
		with Not_found -> Version in
	let src_name =
		if Array.length Sys.argv > 2 then Sys.argv.(2)
		else "stdin" in
	match action with
	| Ast ->
		print_string ("Printing AST of program " ^ src_name ^ " ...\n");
		let lexbuf =
		if Array.length Sys.argv > 2 then
			Lexing.from_channel (open_in Sys.argv.(2))
		else
			Lexing.from_channel stdin in
		let ast = Parser.program Scanner.token_parser lexbuf in
		print_ast ast;
	| Compile
	| Translate as flag ->
		print_string ("Translating program " ^ src_name ^ " ...\n");
		let lexbuf =
		if Array.length Sys.argv > 2 then
			Lexing.from_channel (open_in Sys.argv.(2))
		else
			Lexing.from_channel stdin in
		let ast = Parser.program Scanner.token_parser lexbuf in
		let translate_name =
			String.sub Sys.argv.(2) 0 (String.index Sys.argv.(2) '.') in
		let java_src =
			begin
  			try
    			Semantic.check_semantic ast;
    		with Failure msg -> print_string msg; exit 0;
			end;
			translate_ast translate_name ast in
		let translate_chn = open_out (translate_name ^ ".java") in
		output_string translate_chn java_src;
		flush translate_chn;
		print_string ("Written to " ^ translate_name ^ ".java.\n");
		if flag == Compile then
			begin
    		print_string ("Compiling program " ^ src_name ^ " ...\n");
				flush stdout;
    		Unix.system ("javac " ^ translate_name ^ ".java; rm -rf " ^ translate_name ^ ".java"); ()
			end
		else ()
	| Version -> print_version ();;