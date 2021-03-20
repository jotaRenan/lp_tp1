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
    val resultType = teval(e, typesEnv);
    val resultVar = eval(e, varsEnv)
in
    val2string(resultVar) ^ " : " ^ type2string(resultType)
end

val e = fromString "fun rec f1(Int x):Int = x + 1; f1(12)";
run e;