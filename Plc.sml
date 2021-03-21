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
handle Impossible => "It is impossible to execute this function."
 | HDEmptySeq => "Can't apply head function to an empty seq."
 | TLEmptySeq => "Can't apply tail function to an empty seq."
 | ValueNotFoundInMatch => "Value not found in match and no default specified."
 | NotAFunc => "Can't execute an operation that isn't a function."
 | SymbolNotFound => "This operation depends on an undefined free var."
 | EmptySeq => "Sequence contains no elements."
 | UnknownType => "Unknown type specified."
 | NotEqTypes => "Types used in comparison differ."
 | WrongRetType => "Specified function return type is incompatible with its body."
 | DiffBrTypes => "Branches return types differ."
 | IfCondNotBool => "Condition used in If Expression is not boolean."
 | NoMatchResults => "No results specified in Match expression."
 | MatchResTypeDiff => "Return types differ in match conditions."
 | MatchCondTypesDiff => "Match condition types differ."
 | CallTypeMisM => "Can't execute this type."
 | NotFunc => "Parameter is not a function."
 | ListOutOfRange => "List index out of bounds"
 | OpNonList => "Tried to access an element in an expression that is not a list."
