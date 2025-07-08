(* CS443, Fall 2022 *)
(* Project 5 *)
(* Andrew Chang-DeWitt *)

open LLVM.Ast
module VMap = LLVM.Typecheck.LLVarmap

module ExpDataflow = Dataflow.Make
                       (struct type t = var end)
                       (struct type t = inst
                               let compare a b =
                                 (* This may not do the right thing, but it'll
                                  * do something, which is good enough to just
                                  * treat the set like a list *)
                                 if a < b then -1 else
                                   if a = b then 0 else 1
                        end)

module DFG = ExpDataflow.DFG
module ExpSet = ExpDataflow.FSet

(** === Convenience functions === **)
(* You may or may not need these depending on what optimizations you
 * implement, but you should look through them and know what's available! *)

(* Return true iff t1 and t2 are equal types *)
let rec types_eq (t1: typ) (t2: typ) : bool =
  match (t1, t2) with
  | (TInteger i1, TInteger i2) -> i1 = i2
  | (TPointer t1, TPointer t2) -> types_eq t1 t2
  | (TFunction (r1, args1), TFunction (r2, args2)) ->
     types_eq r1 r2 &&
       (try
          List.fold_left2
            (fun b at1 at2 -> b && types_eq at1 at2)
            true
            args1
            args2
        with Invalid_argument _ -> false)
  | (TVoid, TVoid) -> true
  | (TStruct s1, TStruct s2) -> s1 = s2
  | _ -> false

(* If i is "a = e1", this will return Some d for the first instruction
 * "d = e2" in fs where e1 = e2.
 * Equality is determined by inst_rhs_eq.
 * Returns None if there is no matching instruction in fs.
 *)
let get_dest_of_matching_exp (i: inst) (fs: ExpSet.t) : var option =
  let val_eq v1 v2 =
    match (v1, v2) with
    | (Const n1, Const n2) -> n1 = n2
    | (Var n1, Var n2) -> compare_var n1 n2 = 0
    | _ -> false
  in
  let inst_rhs_eq i1 i2 =
    match (i1, i2) with
    | (ISet (_, _, v1), ISet (_, _, v2)) -> val_eq v1 v2
    | (IBinop (_, b1, _, v11, v12), IBinop (_, b2, _, v21, v22)) ->
       val_eq v11 v21 && val_eq v12 v22 && b1 = b2
    | (ICmp (_, b1, _, v11, v12), ICmp (_, b2, _, v21, v22)) ->
       val_eq v11 v21 && val_eq v12 v22 && b1 = b2
    | (ICast (_, c1, _, v1, t1), ICast (_, c2, _, v2, t2)) ->
       c1 = c2 && types_eq t1 t2 && val_eq v1 v2
    | (IGetElementPtr (_, _, v1, inds1), IGetElementPtr (_, _, v2, inds2)) ->
       compare_var v1 v2 = 0 &&
         (try
            List.fold_left2
              (fun b (_, v1) (_, v2) -> b && val_eq v1 v2)
              true
              inds1
              inds2
          with Invalid_argument _ -> false)
    | (ILoad (_, _, v1), ILoad (_, _, v2)) ->
       (* This is OK as long as stores kill any loads *)
       compare_var v1 v2 = 0
    | _ -> false
  in
  ExpSet.fold
    (fun i2 a ->
      if inst_rhs_eq i i2 then
        match LLVM.Utils.def_inst i2 with
        | d::_ -> Some d
        | _ -> a
      else a)
    fs
    None

(* Get the type of a variable giving the LLVM typing context *)
let get_type_of_var (ts: typ LLVM.Typecheck.LLVarmap.t) (v: var) =
  VMap.find (LLVM.SSA.base_var v) ts

(* Perform substitutions on every value in a list of LLVM instructions.
* rep_val will be called on every value operand and the returned value will be
* used in its place. This is also done on variable operands
* (e.g. f in "call ty f(args)"), but variable operands will only be replaced 
* if the replacement is also a variable.
* This is sort of like "map" on LLVM function bodies. *)
let sub_body (rep_val: value -> value) (body: inst list) : inst list=
  List.map
    (fun i ->
      match i with
      | ISet (d, t, v) -> ISet (d, t, rep_val v)
      | IBinop (d, b, t, v1, v2) -> IBinop (d, b, t, rep_val v1, rep_val v2)
      | ICmp (d, c, t, v1, v2) -> ICmp (d, c, t, rep_val v1, rep_val v2)
      | ICast (d, ct, t, v, t2) -> ICast (d, ct, t, rep_val v, t2)
      | ICondBr (v, tl, fl) -> ICondBr (rep_val v, tl, fl)
      | IRet (Some (t, v)) -> IRet (Some (t, rep_val v))
      | ICall (v1, t, v2, args) ->
         ICall (v1, t,
                (match rep_val (Var v2) with
                 | Var v2' -> v2'
                 | _ -> v2),
                List.map (fun (t, v) -> (t, rep_val v)) args)
      | IGetElementPtr (v1, t, v2, inds) ->
         IGetElementPtr
           (v1, t,
            (match rep_val (Var v2) with
            | Var v2' -> v2'
            | _ -> v2),
            List.map (fun (t, v) -> (t, rep_val v)) inds)
      | ILoad (d, t, v) -> ILoad (d, t,
                                  (match rep_val (Var v) with
                                   | Var v' -> v'
                                   | _ -> v))
      | IStore (t, vl, v) -> IStore (t, rep_val vl,
                                     (match rep_val (Var v) with
                                      | Var v' -> v'
                                      | _ -> v))
      | IPhi (d, t, preds) ->
         IPhi (d, t, List.map (fun (l, v) -> (l, rep_val v)) preds)
      | _ -> i
    )
    body

type func_optimizer = typ LLVM.Typecheck.LLVarmap.t -> string -> inst list
                      -> inst list

let rec iterate (cond: int option)
          (funs: func_optimizer list)
          (ts: typ LLVM.Typecheck.LLVarmap.t)
          (fname: string)
          (body: inst list) : inst list =
  let body' =
    List.fold_left (fun body f -> f ts fname body) body funs
  in
  match cond with
  | Some n ->
     if n <= 1 then body'
     else iterate (Some (n - 1)) funs ts fname body'
  | None ->
     if List.length body' < List.length body then
       iterate None funs ts fname body'
     else body

let opt_body (ts: typ LLVM.Typecheck.LLVarmap.t) (fname: string)
      (body: inst list) : inst list =
  body
  
let opt_func ts f =
  make_func
    f.f_name
    f.f_ret
    f.f_args
    (opt_body ts f.f_name (Array.to_list f.f_body))

let opt (ts: typ LLVM.Typecheck.LLVarmap.t) (prog: prog) =
  List.map (opt_func ts) prog
