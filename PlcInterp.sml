(* PlcInterp *)

use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun deconstructIntT(v: plcVal) : int =
    case v of 
        IntV x => x
        | _ => raise Impossible

fun eval(e: expr, ro: plcVal env) : plcVal =
    case e of
        ConI e1 => IntV e1
        | ConB e1 => BoolV e1 
        | Prim2("+", ConI e1, ConI e2) => IntV(e1 + e2)
        | Prim2("-", ConI e1, ConI e2) => IntV(e1 - e2)
        | Prim2("*", ConI e1, ConI e2) => IntV(e1 * e2)
        | Prim2("/", ConI e1, ConI e2) => IntV(e1 div e2)
        | Prim2(operation, e1, e2) => let
          val ve1 = deconstructIntT(eval(e1, ro));
          val ve2 = deconstructIntT(eval(e2, ro));
        in
          eval(Prim2(operation, ConI ve1, ConI ve2), ro)
        end
        | _ => raise NotAFunc;

val expr0 = Prim2("+", ConI 2, ConI 3);
val expr1 = Prim2("-", ConI 2, ConI 3);
val expr2 = Prim2("/", ConI 6, ConI 3);
val expr3 = Prim2("*", ConI 2, ConI 3);

eval(expr0, []);
eval(expr1, []);
eval(expr2, []);
eval(expr3, []);