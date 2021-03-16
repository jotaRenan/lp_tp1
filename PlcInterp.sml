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

fun deconstructSeqV(v: plcVal) : plcVal list =
    case v of 
        SeqV x => x
        | _ => raise Impossible

fun deconstructVar(v: expr) : string =
    case v of 
        Var x => x
        | _ => raise Impossible
  
fun eval(e: expr, ro: plcVal env) : plcVal =
    case e of
        ConI e1 => IntV e1
        | ConB e1 => BoolV e1 
        | List [] => ListV []
        | Var x => lookup ro x
        | List(x::xs: expr list) => let
          val e1 = eval(x, ro);
          val e2 = deconstructListV(eval(List xs, ro));
        in
          ListV (e1::e2)
        end
        | Prim2(";", e1, e2) => let
          val e1 = eval(e1, ro)
        in
          eval(e2, ro)
        end
        | Prim2("::", e1, ESeq _) => SeqV(eval(e1, ro)::[])
        | Prim2("::", e1, e2) => let
          val ve2 = deconstructSeqV(eval(e2, ro));
          val ve1 = eval(e1, ro);
        in
          SeqV(ve1::ve2)
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
        | If(e1, e2, e3) => if eval(e1, ro) = BoolV true then eval(e2, ro) else eval(e3, ro)
        | Prim1("print", e1) => let
          val ve1 = eval(e1, ro);
          val str = val2string(ve1);
        in
          print (str ^ "\n");
          ListV []
        end
        | Anon(e1, e2, e3) => Clos("", e2, e3, ro)
        | Let(e1, e2, e3) => let
          val ve2 = eval(e2, ro)
        in
          eval(e3, (e1, ve2)::ro)
        end
        (* | (Call(f, e)) => let 
          val vf = eval(f, ro);
          val fv = lookup ro vf
        in
          case fv of
            (Clos(f, x, e1, fSt)) => let
              val ve1 = eval(e, ro);
              val ro' = (x, ve1) :: (f, fv) :: ro
            in
                eval(e1, ro')
            end
            | _ => raise Impossible
        end  *)
        | _ => raise NotAFunc;

val expr0 = Prim2("+", ConI 2, ConI 3);
val expr1 = Prim2("-", ConI 2, ConI 3);
val expr2 = Prim2("/", ConI 6, ConI 3);
val expr3 = Prim2("*", ConI 2, ConI 3);
val expr4 = List([ConI 6, ConB false]);
val expr5 = Item(1, List([ConI 6, ConB false]));
val expr6 = ESeq(SeqT BoolT);
val expr7 = Prim1("print", ConI 27);
val expr8 = Prim2 ("::", ConI 3, Prim2 ("::", ConI 4, Prim2 ("::", ConI 5, ESeq (SeqT IntT))));
val expr9 = Prim2 (";", Prim1 ("print", ConI 27), ConB true);
val expr10 = Anon (IntT, "x", Prim1("-", Var "x"));
val expr11 = Let("x", ConI 9, Prim2 ("+", Var "x", ConI 1));
(* val expr12 = Let("f", Anon (Int, "x", Var "x"), Call ("f", ConI 1)); *)
val expr13 = If(ConB true, ConI 1, ConI 2);

eval(expr0, []);
eval(expr1, []);
eval(expr2, []);
eval(expr3, []);
eval(expr4, []);
eval(expr5, []);
eval(expr6, []);
eval(expr7, []);
eval(expr8, []);
eval(expr9, []);
eval(expr10, []);
eval(expr11, []);
(* eval(expr12, []); *)
eval(expr13, []);
