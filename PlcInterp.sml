(* PlcInterp *)

use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun deconstructIntV(v: plcVal) : int =
    case v of 
        IntV x => x
        | _ => raise Impossible

fun deconstructListV(v: plcVal) : plcVal list =
    case v of 
        ListV x => x
        | _ => raise Impossible
  
fun eval(e: expr, ro: plcVal env) : plcVal =
    case e of
        ConI e1 => IntV e1
        | ConB e1 => BoolV e1 
        | List [] => ListV []
        | List e => let
          val x::xs = e;
          val e1 = eval(x, ro);
          val e2 = deconstructListV(eval(List xs, ro));
        in
          ListV ([e1] @ e2)
        end
        | Prim2("+", ConI e1, ConI e2) => IntV(e1 + e2)
        | Prim2("-", ConI e1, ConI e2) => IntV(e1 - e2)
        | Prim2("*", ConI e1, ConI e2) => IntV(e1 * e2)
        | Prim2("/", ConI e1, ConI e2) => IntV(e1 div e2)
        | Prim2(operation, e1, e2) => let
          val ve1 = deconstructIntV(eval(e1, ro));
          val ve2 = deconstructIntV(eval(e2, ro));
        in
          eval(Prim2(operation, ConI ve1, ConI ve2), ro)
        end
        | Item(e1, List e2) => eval(List.nth(e2, e1-1), ro)
        | ESeq _ => SeqV []
        | Prim1("print", e1) => let
          val ve1 = eval(e1, ro);
          val str = val2string(ve1);
        in
          print (str ^ "\n");
          ListV []
        end
        | _ => raise NotAFunc;

val expr0 = Prim2("+", ConI 2, ConI 3);
val expr1 = Prim2("-", ConI 2, ConI 3);
val expr2 = Prim2("/", ConI 6, ConI 3);
val expr3 = Prim2("*", ConI 2, ConI 3);
val expr4 = List([ConI 6, ConB false]);
val expr5 = Item(1, List([ConI 6, ConB false]));
val expr6 = ESeq(SeqT BoolT);
val expr7 = Prim1("print", ConI 27);

eval(expr0, []);
eval(expr1, []);
eval(expr2, []);
eval(expr3, []);
eval(expr4, []);
eval(expr5, []);
eval(expr6, []);
eval(expr7, []);