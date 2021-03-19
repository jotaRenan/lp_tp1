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
        | Prim2(";", e1, e2) => let
          val t1 = teval(e1, ro);
          val t2 = teval(e2, ro);
        in
          t2
        end
        | Prim2("&&", e1, e2) => if (teval(e1, ro) = BoolT andalso teval(e2, ro) = BoolT) then BoolT else raise NotEqTypes
        | Prim2("::", e1, ESeq(SeqT t2)) => let
          val t1 = teval(e1, ro);         
        in
           if t1 = t2 then SeqT(t1) else raise NotEqTypes
        end
        | Prim2("::", e1, e2) => let
          val t1 = teval(e1, ro);
          val t2 = teval(e2, ro);
        in
           if SeqT(t1) = t2 then SeqT(t1) else raise NotEqTypes
        end
        | List([]) => ListT([])
        | List(h::t: expr list) => ListT(teval(h, ro)::(deconstructListT(teval(List(t), ro))))
        | Call(Var(e2), e1) => 
            let
                val FunT(argT, retT) = lookup ro e2;
                val t1 = teval(e1, ro);
            in 
                if t1 = argT then retT else raise CallTypeMisM 
            end
        | Call(Call a, e1) =>  let val FunT(_, r) = teval(Call a, ro) in r end 
        | Call(_, _) => raise NotFunc
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
        | Anon(t, name, e1) => FunT(t, teval(e1, (name, t)::ro))
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
        | Letrec(funName, argT, varName, retT, e1, e2) => 
        let
            val t1 = teval(e1, (funName, FunT(argT, retT))::(varName, argT)::ro);
            val t2 = teval(e2, (funName, FunT(argT, retT))::ro);
        in
            t2
        end
        | _ => raise UnknownType;


(* DEU ERRO: *)
(* val expr0 = Let ("p",List [ConI 1, ConI 3], Let ("f", Anon (ListT [IntT, IntT], "$list", Let ("x",Item (1, Var "$list"), Let ("y",Item (2, Var "$list"),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",Var "p"))); *)
(* val expr0 = Let("inc",Anon (IntT,"x",Prim2 ("+",Var "x",ConI 1)), Let ("add",Anon(ListT [IntT, IntT],"$list", Let ("x",Item (1,Var "$list"),Let ("y",Item (2,Var "$list"),Prim2 ("+",Var "x",Var "y")))),Let("cadd",Anon (IntT,"x",Anon (IntT,"y",Prim2 ("+",Var "x",Var "y"))), Let ("y",Call (Var "add",List [ConI 3, Call (Var "inc",ConI 4)]),Let("x", Call (Call (Var "cadd",ConI 3),Prim2 ("-",ConI 7,Var "y")), Let ("z",Prim2 ("*",Var "x",ConI 3),Letrec("fac",IntT,"n",IntT, Match (Var "n",[(SOME (ConI 0), ConI 1), (SOME (ConI 1), ConI 1), (NONE,Prim2("*",Var "n", Call (Var "fac",Prim2 ("-",Var "n",ConI 1))))]), Prim2 (";",Prim1 ("print",Var "x"),Prim2(";",Prim1 ("print",Var "y"), Prim2 ("::",Var "x",Prim2("::",Var "y", Prim2 ("::",Var "z",Prim2("::",Call (Var "fac",Var "z"), ESeq (SeqT IntT)))))))))))))); *)
(* val expr0 = Letrec ("map",FunT (IntT,IntT), "f", FunT (SeqT IntT,SeqT IntT), Anon(SeqT IntT, "l",If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),Call(Call (Var "map",Anon (IntT, "x",Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,ESeq (SeqT IntT)))))); *)
(* val expr0 = Anon (ListT [IntT, IntT], "$list", Let ("x",Item (1, Var "$list"), Let ("y",Item (2, Var "$list"),Prim2 ("-",Var "x",Var "y")))); *)
(* val expr0 = Item (2, Item (1, List [List [ConI 5, ConI 6], ConB false])); *)

(* VERIFICAR SE O RETORNO DA REGRA 21 DEVE SER SEQT OU LISTT
val expr0 = Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,ESeq (SeqT IntT))); *)
teval(expr0, []);