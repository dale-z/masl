open Ast;;

(* Environment: symbol tables, jump tables, etc. *)

type s_type_spec =
	  ValidType of type_spec
	| InvalidType;;

type s_expr = expr * s_type_spec;;

type s_var_decl = {
	id: string;
	t: type_spec;
};;

type s_symbol_table = {
	parent: symbol_table option;
	var_decls: var_decl list;
};;

type s_translate_env = {
	scope: symbol_table;
	cur_func: type_spec option;
	value_returned: bool;
	in_object: bool;
	in_loop: bool;
};;

let rec s_get_var_decl id sym_t =
	try
		let vd = List.find (fun vd -> vd.id = id) sym_t.var_decls
		in vd.type_spec
	with Not_found -> InvalidType;;

let rec s_get_type expr = match expr with
    Id(id) -> InvalidType (* Look up symbol table and check it out. *)
	| BasicLit(basic_type, _) -> ValidType(basic_type)
	| FuncLit(lit) -> InvalidType (* Parse it. *)
	| ObjectLit -> ValidType(Object)
	| This -> ValidType(Object)
	| UnaryOp -> InvalidType (* Go down to parse it. *)
	| BinaryOp -> InvalidType (* Go down to parse it. *)
	| FuncCall -> InvalidType (* Find the function *)
	| NoExpr -> Void;;

