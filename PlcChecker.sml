(* PlcChecker *)

use "Environ.sml";
use "Absyn.sml";

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList


fun teval(e: expr, ro: plcType env) : plcType =
    case e of
        ConI _ => IntT
        | ConB _ => BoolT
        | Prim1("!", e1) => if teval(e1, ro) = BoolT then BoolT else raise NotEqTypes
        (* | Prim1("hd", e1) => if teval(e1, ro) = then BoolT else raise NotEqTypes *)
        | Prim2("&&", e1, e2) => if (teval(e1, ro) = BoolT andalso teval(e2, ro) = BoolT) then BoolT else raise NotEqTypes
        | Prim2("::", e1, e2) => let
          val t1 = teval(e1, ro);
          val t2 = teval(e2, ro);
        in
            (case t1 of
                SeqT t => if t = t2 then SeqT t else raise NotEqTypes
                | _ => raise OpNonList
            )
        end
        | List([]) => ListT([])
        (* | List(h::t) => teval(h, ro)::teval(List t, ro)::[] *)
        | Match(e: expr, conds: (expr option * expr) list) => if VERIFICAR then teval((hd conds), ro) else raise DiffBrTypes
        | If(e1, e2, e3) => let
            val t2 = teval(e2, ro);
            val t3 = teval(e3, ro);
        in
            if (teval(e1, ro) = BoolT) 
            then (
                if t2 = t3 
                then t2 
                else raise DiffBrTypes
            ) 
            else raise IfCondNotBool
        end
        | Prim1("-", e1) => if teval(e1, ro) = IntT then IntT else raise NotEqTypes
        | Prim2(ope, e1, e2) =>
            (case ope of
                ("+" | "-" | "*" | "/") => let
                    val t1 = teval(e1, ro);
                    val t2 = teval(e2, ro);
                in
                    if t1 = t2 andalso t1 = IntT then 
                        IntT 
                    else raise NotEqTypes
                end
                | ("<" | "<=") => let
                    val t1 = teval(e1, ro);
                    val t2 = teval(e2, ro);
                in
                    if t1 = t2 andalso t1 = IntT then 
                        BoolT 
                    else raise NotEqTypes
                end
                | ("=" | "!=") => let
                        val t1 = teval(e1, ro);
                        val t2 = teval(e2, ro);
                    in
                        if (t1 = IntT orelse t1 = BoolT) andalso t1 = t2 then BoolT else raise NotEqTypes
                    end
                | _ => raise UnknownType)
        | ESeq(SeqT(t)) => t
        | Anon(t, name, e1) => teval(e1, (name, t)::ro)
        | Prim1("print", e1) => let
          val t1 = teval(e1, ro);
        in
          ListT([])
        end
        | Prim1("ise", e1) => 
            (case teval(e1, ro) of
                ListT _ => BoolT
                | SeqT _ => BoolT
                | _ => raise OpNonList
            )
        | Let(name, e1, e2) => teval(e2, (name, teval(e1, ro))::ro)
        | _ => raise UnknownType;

fun tudoIgual (conds: (expr option * expr) list) = [] 


val expr0 = List([ConI 11, ConI 9, ConI 29, ConB false]);
val expr4 = Prim1("ise", List([ConI 11, ConI 9, ConI 29, ConB false]));
(* val expr1 = If(Prim2("=", ConI 11, ConI 0), ConI 1, ConI 0);
val expr2 = Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true));
val expr3 = Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4)); *)

teval(expr4, []);