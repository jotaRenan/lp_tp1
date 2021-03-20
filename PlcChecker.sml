(* PlcChecker *)

(* use "Environ.sml";
use "Absyn.sml"; *)

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
        | _ => raise OpNonList

fun areAllRetTypesEqual (retTypes: plcType list) = foldl (fn (a, (areAllSame, t1)) => (areAllSame andalso t1 = a, t1)) (true, (hd retTypes)) retTypes;
fun notNone (expa, _) : bool = case expa of NONE => false | SOME x => true;

fun teval(e: expr) (ro: plcType env) : plcType =
    case e of
        ConI _ => IntT
        | ConB _ => BoolT
        | Var a => (lookup ro a)
        | Prim1("!", e1) => if teval e1 ro = BoolT then BoolT else raise UnknownType
        | Prim1("hd", e1) => 
            let
                val t1 = teval e1 ro;
            in
                case t1 of SeqT x1 => x1 
                    | _ => raise OpNonList
            end
        | Prim1("tl", e1) => 
            let
                val t1 = teval e1 ro;
            in
                case t1 of SeqT x1 => t1
                    | _ => raise OpNonList
            end
        | Prim2(";", e1, e2) => let
          val t1 = teval e1 ro;
          val t2 = teval e2 ro;
        in
          t2
        end
        | Prim2("&&", e1, e2) => if (teval e1 ro = BoolT andalso teval e2 ro = BoolT) then BoolT else raise NotEqTypes
        | Prim2("::", e1, ESeq(SeqT t2)) => let
          val t1 = teval e1 ro;         
        in
           if t1 = t2 then SeqT(t1) else raise NotEqTypes
        end
        | Prim2("::", e1, e2) => let
          val t1 = teval e1 ro;
          val t2 = teval e2 ro;
        in
           if SeqT(t1) = t2 then SeqT(t1) else raise NotEqTypes
        end
        | List([]) => ListT([])
        | List(h::t: expr list) => ListT((teval h ro)::(deconstructListT(teval (List(t)) ro)))
        | Call(Var(e2), e1) => 
            let
                val mayBeFunType = lookup ro e2;
                val t1 = teval e1 ro;
            in 
                case mayBeFunType of FunT(argT, retT) => if t1 = argT then retT else raise CallTypeMisM 
                    | _ => raise NotFunc
            end
        | Call(Call a, e1) => 
            let 
                val x = teval (Call a) ro 
            in 
                case x of FunT(_, r) => r 
                    | _ => raise NotFunc
            end 
        | Call(_, _) => raise NotFunc
        | Match(_, []) => raise NoMatchResults
        | Match(e, conds: (expr option * expr) list) => 
        let
            val mapCondsToRetsTypes = fn x => (map (fn (_, r) => teval r ro) x);
            val mapCondsToCondExpTypes = fn x => (map (fn (SOME c, _) => teval c ro | (_,_) => raise UnknownType) x);
            val (allSame, tRes) = areAllRetTypesEqual(mapCondsToRetsTypes conds);
            val condsExceptNone = (List.filter notNone conds); 
            val condTypes = mapCondsToCondExpTypes (condsExceptNone);
            val (allCondSame, tCond) = areAllRetTypesEqual(condTypes);
            val eType = teval e ro;
        in
            if allSame = false 
            then raise MatchResTypeDiff 
            else if (allCondSame = false orelse eType <> tCond)
            then raise MatchCondTypesDiff
            else tRes
        end
        | If(e1, e2, e3) => let
            val t2 = teval e2 ro;
            val t3 = teval e3 ro;
        in
            if (teval e1 ro = BoolT) 
            then (
                if t2 = t3 
                then t2 
                else raise DiffBrTypes
            ) 
            else raise IfCondNotBool
        end
        | Item(_, List []) => raise EmptySeq
        | Item(i, List e1) => 
            if 
                (0 < i andalso i <= List.length(e1)) 
            then 
                teval (List.nth(e1, i - 1)) ro
            else raise ListOutOfRange
        | Item(i,  e1) => 
            let
                val t1 = teval e1 ro;
            in
                case t1 of 
                    ListT li => if (0 < i andalso i <= List.length(li)) then List.nth(li, i - 1) else raise ListOutOfRange
                    | _ => raise OpNonList
            end
        | Prim1("-", e1) => if teval e1 ro = IntT then IntT else raise UnknownType
        | Prim2(ope, e1, e2) =>
            (case ope of
                ("+" | "-" | "*" | "/") => let
                    val t1 = teval e1 ro;
                    val t2 = teval e2 ro;
                in
                    if t1 = t2 andalso t1 = IntT then 
                        IntT 
                    else raise NotEqTypes
                end
                | ("<" | "<=") => let
                    val t1 = teval e1 ro;
                    val t2 = teval e2 ro;
                in
                    if t1 = t2 andalso t1 = IntT then 
                        BoolT 
                    else raise NotEqTypes
                end
                | ("=" | "!=") => let
                        val t1 = teval e1 ro;
                        val t2 = teval e2 ro;
                    in
                        if (t1 = IntT orelse t1 = BoolT orelse t1 = (ListT [])) andalso t1 = t2 then BoolT else raise NotEqTypes
                    end
                | _ => raise UnknownType)
        | ESeq(SeqT(t)) => t
        | Anon(t, name, e1) => FunT(t, teval e1 ((name, t)::ro))
        | Prim1("print", e1) => let
          val t1 = teval e1 ro;
        in
          ListT([])
        end
        | Prim1("ise", e1) => 
            let
                val t1 = teval e1 ro;
            in
                case t1 of SeqT _ => BoolT
                    | _ => raise UnknownType
            end
        | Let(name, e1, e2) => teval e2 ((name, teval e1 ro)::ro)
        | Letrec(funName, argT, varName, retT, e1, e2) => 
        let
            val t1 = teval e1 ((funName, FunT(argT, retT))::(varName, argT)::ro);
            val t2 = teval e2 ((funName, FunT(argT, retT))::ro);
        in
            if retT = t1 then t2 else raise WrongRetType
        end
        | _ => raise UnknownType;
