(*
  Make sure you regenerate the Parser and Lexer
  every time you modify PlcLexer.fsl or PlcParser.fsy
*)

// Windows only
#r "C:\Users\\trist\Desktop\project\\bin\FsLexYacc.Runtime.dll"
#load "C:\Users\\trist\Desktop\project\Environ.fs"
#load "C:\Users\\trist\Desktop\project\Absyn.fs"
#load "C:\Users\\trist\Desktop\project\PlcParserAux.fs"
#load "C:\Users\\trist\Desktop\project\PlcParser.fs"
#load "C:\Users\\trist\Desktop\project\PlcLexer.fs"
#load "C:\Users\\trist\Desktop\project\Parse.fs"
#load "C:\Users\\trist\Desktop\project\PlcInterp.fs"
#load "C:\Users\\trist\Desktop\project\PlcChecker.fs"
#load "C:\Users\\trist\Desktop\project\Plc.fs"

open Absyn
let fromString = Parse.fromString // string parser function
let run e = printfn "\nResult is  %s\n" (Plc.run e)   // execution function

(* Examples in concrete syntax *)

let e1 = Parse.fromString "
15
"
run e1

let e1 = fromString "
true
"
run e1

let e1 = fromString "
()
"
run e1

let e1 = fromString "
(6, false)[1]
"
run e1

let e1 = fromString "
([Bool][])
"
run e1


let e1 = fromString "
print x; true
"
run e1

let e1 = fromString "
3::7::t
"
run e1

let e1 = fromString "
fn (Int x) => -x end
"
run e1

let e1 = fromString "
var x = 9; x + 1
"
run e1

let e1 = fromString "
fun f(Int x) = x; f(1)
"
run e1

let e1 = fromString "
match x with
| 0 -> 1
| _ -> -1
end
"
run e1

let e1 = fromString "
fun rec f(Int n) = ,
if n <= 0 then 0 
else n + f(n-1) ; 
f(5) 
"
run e1