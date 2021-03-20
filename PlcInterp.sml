(* PlcInterp *)

(* use "Environ.sml";
use "Absyn.sml"; *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun deconstructIntV(v: plcVal) : int =
    case v of 
        IntV x => x
        | _ => raise Impossible

fun deconstructBoolV(v: plcVal) : bool =
    case v of 
        BoolV x => x
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

fun deconstructBoolV(v: plcVal) : bool =
    case v of 
        BoolV x => x
        | _ => raise Impossible

fun getTail(l: plcVal list) : plcVal =
  case l of
    (t::[]) => t
    | (x::ts: plcVal list) => getTail(ts)
    | _ => raise TLEmptySeq

  
fun eval(e: expr) (ro: plcVal env) : plcVal =
    case e of
        ConI e1 => IntV e1
        | ConB e1 => BoolV e1 
        | Var x => lookup ro x
        | ESeq _ => SeqV []
        | List [] => ListV []
        | List(x::xs: expr list) => let
          val e1 = eval x ro;
          val e2 = deconstructListV(eval(List xs) ro);
        in
          ListV (e1::e2)
        end
        | Prim2("&&", ConB e1, ConB e2) => BoolV(e1 andalso e2)
        | Prim2("&&", e1, e2) => let
          val e1b = deconstructBoolV(eval e1 ro);
          val e2b = deconstructBoolV(eval e2 ro)
        in
          eval (Prim2("&&", ConB(e1b), ConB(e2b))) ro
        end 
        | Prim2("::", e1, ESeq _) => SeqV((eval e1 ro)::[])
        | Prim2("::", e1, e2) => let
          val ve2 = deconstructSeqV(eval e2 ro);
          val ve1 = eval e1 ro;
        in
          SeqV(ve1::ve2)
        end
        | Prim2(";", e1, e2) => let
          val e1 = eval e1 ro
        in
          eval e2 ro
        end
        | Prim2("+", ConI e1, ConI e2) => IntV(e1 + e2)
        | Prim2("-", ConI e1, ConI e2) => IntV(e1 - e2)
        | Prim2("*", ConI e1, ConI e2) => IntV(e1 * e2)
        | Prim2("/", ConI e1, ConI e2) => IntV(e1 div e2)
        | Prim2("<=", ConI e1, ConI e2) => BoolV(e1 <= e2)
        | Prim2("<", ConI e1, ConI e2) => BoolV(e1 < e2)
        | Prim2("=", e1, e2) => BoolV(e1 = e2)
        | Prim2("!=", e1, e2) => BoolV(not (e1 = e2))
        | Prim2(operation, e1, e2) => let
          val ve1 = deconstructIntV(eval e1 ro);
          val ve2 = deconstructIntV(eval e2 ro);
        in
          eval (Prim2(operation, ConI ve1, ConI ve2)) ro
        end
        | Item(e1, List e2) => eval (List.nth(e2, e1-1)) ro
        | If(e1, e2, e3) => if eval e1 ro = BoolV true then eval e2 ro else eval e3 ro
        | Prim1("!", ConB e1) => BoolV(not e1)
        | Prim1("!", e1) => BoolV(not(deconstructBoolV(eval e1 ro))) 
        | Prim1("-", ConI e1) => IntV(~e1)
        | Prim1("-", e1) => eval(Prim1("-", ConI(deconstructIntV(eval e1 ro)))) ro
        | Prim1("hd", e1) => let
          val ve1 = eval e1 ro
        in
          case ve1 of
            SeqV (x::ts: plcVal list) => x
            | SeqV [] => raise HDEmptySeq
            | _ => raise Impossible
        end
        | Prim1("tl", e1) => let
          val ve1 = eval e1 ro
        in
          case ve1 of
            SeqV l => getTail(l)
            | _ => raise Impossible
        end
        | Prim1("ise", e1) => let
          val ve1 = eval e1 ro
        in
          case ve1 of
            SeqV [] => BoolV true
            | _ => BoolV false
        end
        | Prim1("print", e1) => let
          val ve1 = eval e1 ro;
          val str = val2string(ve1);
        in
          print (str ^ "\n");
          ListV []
        end
        | Match(e1, hd::options: (expr option * expr) list) => let
          val ve1 = eval e1 ro;
          val (m, a) = hd
        in
          case m of
            SOME e2 => if ve1 = eval e2 ro then 
                        eval a ro 
                      else if options = [] then 
                        raise ValueNotFoundInMatch 
                      else 
                        eval(Match(e1, options)) ro
            | NONE => eval a ro
        end
        | Anon(e1, e2, e3) => Clos("", e2, e3, ro)
        | Let(e1, e2, e3) => let
          val ve2 = eval e2 ro
        in
          eval e3 ((e1, ve2)::ro)
        end
        | Letrec(nf, _, nv, _, e3, e4) => let
          val clo = Clos(nf, nv, e3, ro);
          val ro' = (nf, clo)::ro;
        in
          eval e4 ro'
        end
        | (Call(f, e)) => let 
          val vf = deconstructVar(f);
          val fv = lookup ro vf
        in
          case fv of
            (Clos(f, x, e1, fSt)) => let
              val ve1 = eval e ro;
              val ro' = (x, ve1) :: (f, fv) :: ro
            in
              eval e1 ro'
            end
            | _ => raise Impossible
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
val expr8 = Prim2 ("::", ConI 3, Prim2 ("::", ConI 4, Prim2 ("::", ConI 5, ESeq (SeqT IntT))));
val expr9 = Prim2 (";", Prim1 ("print", ConI 27), ConB true);
val expr10 = Anon (IntT, "x", Prim1("-", Var "x"));
val expr11 = Let("x", ConI 9, Prim2 ("+", Var "x", ConI 1));
val expr12 = Let("f", Anon (IntT, "x", Var "x"), Call (Var "f", ConI 5));
val expr13 = If(ConB true, ConI 1, ConI 2);
val expr14 = Match(Var "x", [(SOME (ConI 0), ConI 1), (SOME (ConI 1), ConI 2),(NONE, Prim1("-", ConI 1))]);
val expr15 = Prim2("&&", ConB true, ConB false);
val expr16 = Prim2("&&", ConB true, Prim2("&&", ConB true, ConB true));
val expr17 = Letrec
      ("f",IntT,"x",IntT,
       If
         (Prim2 ("<=",Var "x",ConI 0),ConI 0,
          Prim2 ("+",Var "x",Call (Var "f",Prim2 ("-",Var "x",ConI 1)))),
       Call (Var "f",ConI 5));
val expr18 = Prim1("!", ConB true);
val expr19 = Prim1("hd", expr8);
val expr20 = Prim1("tl", expr8);
val expr21 = Prim1("ise", expr8);
val expr22 = Prim1("ise", expr6);



eval expr1 [];
eval expr2 [];
eval expr0 [];
eval expr3 [];
eval expr4 [];
eval expr5 [];
eval expr6 [];
eval expr7 [];
eval expr8 [];
eval expr9 [];
eval expr10 [];
eval expr11 [];
eval expr12 [];
eval expr13 [];
eval expr14 [("x", IntV 0)];
eval expr14 [("x", IntV 1)];
eval expr14 [("x", IntV 2)];
eval expr15 [];
eval expr16 [];
eval expr17 [];
eval expr18 [];
eval expr19 [];
eval expr20 [];
eval expr21 [];
eval expr22 [];