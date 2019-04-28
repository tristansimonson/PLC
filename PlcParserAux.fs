module ParAux

open Absyn

let l =  "$list"
let elist =  "()"

let rec makeFunAux (n: int) (xs: (plcType * string) list) (e: expr) : expr =
  match xs with
  |[(t,x)] -> Let (x, Item(n, Var (l)), e)
  | (t,x) :: tail -> Let (x, Item(n, Var (l)), (makeFunAux (n+1) (tail) (e)))
    
let rec makeTypeHelper (l: (plcType * string) list): plcType list =
  match l with
  | [(t, x)] -> [t] 
  | (t,x) :: tail -> [t] @ makeTypeHelper tail

let makeType (l: (plcType * string) list): plcType =
   ListT (makeTypeHelper (l))


let makeFun (f: string) (xs: (plcType * string) list) (rt: plcType) (e1: expr) (e2: expr) : expr =
    match xs with
    | []           -> Letrec (f, ListT [], elist, rt, e1, e2)
    | (t, x) :: [] -> Letrec (f, t, x, rt, e1, e2)
    | _            ->
        let t = makeType xs in
        let e1' = makeFunAux 1 xs e1 in
        Letrec(f, t, l, rt, e1', e2)

let makeAnon (xs: (plcType * string) list) (e: expr) : expr =
    match xs with
    | []           -> Anon (ListT [], elist, e)
    | (t, x) :: [] -> Anon (t, x, e)
    | _            ->
        let t = makeType xs in
        let e' = makeFunAux 1 xs e in
        Anon (t, l, e')
