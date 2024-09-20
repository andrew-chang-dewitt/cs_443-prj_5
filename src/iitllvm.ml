(* MiniIITRAN to LLVM Compiler *)
(* IIT CS 443, Fall 2022 *)
(* Project 2 *)

open IITRAN.Ast
module L = LLVM.Ast

exception CompileError of string * loc

let result_var = L.Local "result"
let itype = L.TInteger 64
let btype = L.TInteger 1

let compile_typ =
  function TInteger | TCharacter -> itype
           | TLogical -> btype

let ctr = ref 0
let new_temp () =
  ctr := !ctr + 1;
  L.Local ("temp" ^ (string_of_int !ctr))

let lctr = ref 0
let new_label () =
  lctr := !lctr + 1;
  "label" ^ (string_of_int !lctr)

let compile_var s = L.Var (L.Local s)

let move (dest: L.var) (typ: L.typ) (value: L.value) =
  L.ISet (dest, typ, value)

exception Unimplemented

(* You're going to want to define some other functions here *)

let rec compile_stmt (s: t_stmt) : L.inst list =
  match s.sdesc with
  | SDecl _ -> []
  | SDo ss -> List.concat (List.map compile_stmt ss)
  | SStop -> [L.IRet (Some (itype, L.Var result_var)); L.ILabel (new_label ())]
  | _ -> raise Unimplemented


let compile_prog (p: t_stmt list) : L.prog =
  [L.make_func "main" itype []
     ((L.ILabel (Config.entry_label_of_fun "main"))::
        (List.concat (List.map compile_stmt p))
      @ [L.IRet (Some (itype, L.Var result_var))]
  )]
