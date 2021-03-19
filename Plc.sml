(* Plc interpreter main file *)

use "PlcChecker.sml";
use "PlcInterp.sml";


fun run(e: expr) : string = let
    val typesEnv: plcType env = [];
    val varsEnv: plcVal env = [];
    val resultType = teval(e, typesEnv);
    val resultVar = eval(e, varsEnv)
in
    val2string(resultVar) ^ " : " ^ type2string(resultType)
end