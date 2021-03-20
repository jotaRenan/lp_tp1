(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";


Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run(e: expr) : string = let
    val typesEnv: plcType env = [];
    val varsEnv: plcVal env = [];
    val resultType = teval e typesEnv;
    val resultVar = eval e varsEnv;
in
    val2string(resultVar) ^ " : " ^ type2string(resultType)
end

val e = fromString "fun rec f1(Int x):Int = x + 1; f1(12)";
run e;

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

run expr1;
run expr2;
run expr0;
run expr3;
run expr4;
run expr5;
run expr6;
run expr7;
run expr8;
run expr9;
run expr10;
run expr11;
run expr12;
run expr13;
run expr14;
run expr14;
run expr14;
run expr15;
run expr16;
run expr17;
run expr18;
run expr19;
run expr20;
run expr21;
run expr22;