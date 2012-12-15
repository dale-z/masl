(* Create translation environment: *)
(* create_env parent_env -> child_env *)
(* Check if current position is contained *)
(* in some loop. *)
(* Linked symbol table. *)
(* *)

open Ast;;

(* translate: string -> Ast.program -> (string * string) list *)

let rec translate sim_name node =
  	match node with
  	| Program(stmts) ->
				"public class " ^ sim_name ^ " extends MaslSimulation {\n" ^
				"  public void init() {\n" ^
				(List.fold_left (fun acc stmt -> acc ^ (translate_stmt "      " stmt)) "" stmts) ^
				"  }\n" ^
				"}\n"
and translate_stmt indent node = match node with
  | BasicDecl(type_spec, decl_list) -> indent ^ "basicdecl\n"
  | FuncDecl(id, expr) -> indent ^ "funcdecl\n"
	| ClassDecl(id, states, stmts) -> indent ^ "classdecl\n"
  | ObjectDecl(id, expr) -> indent ^ "objectdecl\n"
  | Expr(expr) -> indent ^ "expr\n"
  | CompStmt(stmts) -> indent ^ "comp\n"
  | If(pred, then_body, else_body) -> indent ^ "if\n"
  | For(init, pred, update, body) -> indent ^ "for\n"
  | ForEach(type_spec, iter, container, body) -> indent ^ "foreach\n"
  | While(pred, body) -> indent ^ "while\n"
  | DoWhile(body, pred) -> indent ^ "dowhile\n"
  | Continue -> indent ^ "continue\n"
  | Break -> indent ^ "break\n"
  | Return(expr) -> indent ^ "return\n"
  | NoStmt -> indent ^ "nostmt\n"
;;