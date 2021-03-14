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
        | Prim1("hd", e1) => if teval(e1, ro) = then BoolT else raise NotEqTypes
        | Prim2("&&", e1, e2) => if (teval(e1, ro) = BoolT andalso teval(e2, ro) = BoolT) then BoolT else raise NotEqTypes
        | List([]) => ListT([])
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
                "+" => if teval(e1, ro) = teval(e2, ro) then IntT else raise NotEqTypes
                | "-" => if teval(e1, ro) = teval(e2, ro) then IntT else raise NotEqTypes
                | "*" => if teval(e1, ro) = teval(e2, ro) then IntT else raise NotEqTypes
                | "/" => if teval(e1, ro) = teval(e2, ro) then IntT else raise NotEqTypes
                | "<" => if teval(e1, ro) = teval(e2, ro) then BoolT else raise NotEqTypes
                | "<=" => if teval(e1, ro) = teval(e2, ro) then BoolT else raise NotEqTypes
                (* | "=" => if teval(e1, ro) = teval(e2, ro) then BoolT else raise NotEqTypes
                | "!=" => if teval(e1, ro) = teval(e2, ro) then BoolT else raise NotEqTypes *)
                | _ => raise UnknownType)
        | _ => raise UnknownType;


val expr1 = If(Prim2("=", ConI 11, ConI 0), ConI 1, ConI 0);
val expr2 = Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true));
val expr3 = Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4));

teval(expr1, []);