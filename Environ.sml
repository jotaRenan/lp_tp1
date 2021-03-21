(*Environ*)

exception SymbolNotFound

type 'a env = (string * 'a) list

fun lookup [] id = 
let
in
  print (id ^ " free var \n");
  raise SymbolNotFound
end 
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;
