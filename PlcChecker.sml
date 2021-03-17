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


fun deconstructListT(t: plcType) : plcType list =
    case t of 
        ListT x => x
        | _ => raise UnknownType

fun areAllRetTypesEqual (retTypes: plcType list) = foldl (fn (a, (areAllSame, t1)) => (areAllSame andalso t1 = a, t1)) (true, (hd retTypes)) retTypes;
fun notNone (expa, _) : bool = case expa of NONE => false | SOME x => true;

fun teval(e: expr, ro: plcType env) : plcType =
    case e of
        ConI _ => IntT
        | ConB _ => BoolT
        | Var a => (lookup ro a)
        | Prim1("!", e1) => if teval(e1, ro) = BoolT then BoolT else raise NotEqTypes
        | Prim1("hd", e1) => hd (deconstructListT(teval(e1, ro)))
        | Prim1("tl", e1) => teval(e1, ro)
        | Prim2("&&", e1, e2) => if (teval(e1, ro) = BoolT andalso teval(e2, ro) = BoolT) then BoolT else raise NotEqTypes
        | Prim2("::", e1, e2) => let
          val t1 = teval(e1, ro);
          val t2 = teval(e2, ro);
        in
           if ListT([t1]) = t2 then t1 else raise NotEqTypes
        end
        | List([]) => ListT([])
        | List(h::t: expr list) => ListT(teval(h, ro)::(deconstructListT(teval(List(t), ro))))
        | Match(e: expr, conds: (expr option * expr) list) => 
        let
            val mapCondsToRetsTypes = fn x => (map (fn (_, r) => teval(r, ro)) x);
            val mapCondsToCondExpTypes = fn x => (map (fn (SOME c, _) => teval(c, ro)) x);
            val (allSame, tRes) = areAllRetTypesEqual(mapCondsToRetsTypes conds);
            val condsExceptNone = (List.filter notNone conds); 
            val condTypes = mapCondsToCondExpTypes (condsExceptNone);
            val (allCondSame, tCond) = areAllRetTypesEqual(condTypes);
            val eType = teval(e, ro);
        in
            if 
                allSame andalso allCondSame andalso (eType = tCond)
            then 
                eType
            else 
                raise DiffBrTypes
        end
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
        | Item(i,  e1) => List.nth(deconstructListT(teval(e1, ro)), i)
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


(* val expr0 = List([ConI 11, ConI 9, ConI 29, ConB false]); *)
(* val expr0 = Item(0, List([ConB false, ConI 29, ConI 0605])); *)
val expr0 = Match(
    ConI 0, 
    [(SOME(ConI 0), ConB false), (SOME(ConI 29), ConB true), (NONE, ConB false)]
);
(* val expr0 = Prim1("tl", List([ConB false, ConI 9])); *)
(* val expr4 = Prim1("ise", List([ConI 11, ConI 9, ConI 29, ConB false])); *)
(* val expr1 = If(Prim2("=", ConI 11, ConI 0), ConI 1, ConI 0);
val expr2 = Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true));
val expr3 = Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4)); *)

teval(expr0, []);