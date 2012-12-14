open Ast;;

let main () =
    let lexbuf = Lexing.from_channel stdin in
			Parser.program Scanner.token_parser lexbuf;;

print_program (main ());;