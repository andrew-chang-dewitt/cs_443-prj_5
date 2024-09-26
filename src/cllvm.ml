(* IIT CS 443 - Fall 2022 *)
(* C to LLVM Compiler *)
(* Project 3 *)

open C.Ast
module L = LLVM.Ast
open C.Typecheck

exception CompileError of string * loc

let new_temp = L.new_temp
let new_label = L.new_label

let compile_var (s, scope) =
  match scope with
  | Local -> L.Var (L.Local s)
  | Global -> L.Var (L.Global s)

let btype = L.TInteger 1
let ctype = L.TInteger 8
let itype = L.TInteger 32
                 
let rec compile_typ ctx t =
  match t with
  | TVoid -> L.TVoid
  | TBool -> btype
  | TChar -> ctype
  | TInt -> itype
  | TArray t -> L.TPointer (compile_typ ctx t)
  | TStruct s -> L.TPointer (L.TStruct s)
  | TFunction (rt, args) ->
     L.TPointer (L.TFunction
                   (compile_typ ctx rt,
                    List.map (fun (t, _) -> compile_typ ctx t) args))
  
let move (dest: L.var) (typ: L.typ) (value: L.value) =
  L.ISet (dest, typ, value)

let compile_cast (dest: L.var) (in_typ: L.typ) (exp: L.value) (out_typ : L.typ)
  =
  let ct =
    match (in_typ, out_typ) with
    | (L.TInteger n1, L.TInteger n2) ->
       if n1 > n2 then L.CTrunc
       else if n1 = n2 then L.CBitcast
       else L.CZext
    | (L.TPointer _, L.TPointer _) -> L.CBitcast
    | (L.TInteger _, L.TPointer _) -> L.CInttoptr
    | (L.TPointer _, L.TInteger _) -> L.CPtrtoint
    | _ -> failwith "invalid cast"
  in
  [L.ICast (dest, ct, in_typ, exp, out_typ)]

exception Unimplemented
  
let rec compile_stmt
      (ctx: ctx)
      (tds: L.typdefs)
      (break_lbl: L.label option)
      (cont_lbl: L.label option)
      (s: t_stmt): L.inst list =
  match s.sdesc with
  | SDecl (v, _, Some e) ->
     (* Compile e, store to local variable v *)
     raise Unimplemented
  | SDecl _ -> []
  | SBlock ss ->
     List.concat (List.map (compile_stmt ctx tds break_lbl cont_lbl) ss)
  | _ -> raise Unimplemented

let compile_func ctx tds (name, t, body) : L.func =
  match t with
  | TFunction (tret, args) ->
     let tret = compile_typ ctx tret in
     let targs = List.map (fun (t, s) -> (compile_typ ctx t, s)) args in
     L.make_func name tret targs
       ((L.ILabel (Config.entry_label_of_fun name))::(compile_stmt ctx tds None None body)
        @ [match tret with
           | L.TVoid -> L.IRet None
           | _ -> L.IRet (Some (tret, Const 0))])
  | _ -> raise (CompileError ("not a function type", ("", 0)))

let rec compile_def ctx tds d : L.func list =
  match d.ddesc with
  | DFun (s, t, b) -> [compile_func ctx tds (s, t, b)]
  | _ -> []

let compile_prog (ctx, ds) : L.prog * L.typdefs =
  let tds = Varmap.map (List.map (compile_typ ctx)) (get_typedefs ctx) in
  (List.concat (List.map (compile_def ctx tds) ds), tds)
