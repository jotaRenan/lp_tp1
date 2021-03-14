(* PlcInterp *)

use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval(e: expr, ro: plcVal env) : plcVal =
    case e of
        ConI e1 => IntV e1
        | ConB e1 => BoolV e1 
        | Prim2("+", ConI e1, ConI e2) => IntV(e1 + e2)
        | Prim2("+", e1, e2) => let
          val ve1 = eval(e1, ro);
          val ve2 = eval(e2, ro);
        in
          eval(Prim2("+", ve1, ve2), ro)
        end
        | _ => raise NotAFunc;
