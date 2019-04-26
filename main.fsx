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
fun next(Int y): Int = y + 1;
next(6) = 7
"
run e1

let e1 = fromString "
fun next(Int y):Int = y + 1;
next(next(6))
"
run e1

let e1 = fromString "
fun add (): (Int, Int) = (5, 1-4);
add()
"
run e1

let e1 = fromString "
fun g (): (Int, Int, (Int, Int)) = (5, 1-4, (5,5));
g()
"
run e1

let e1 = fromString "
fun add (Int x,Int y):Int = y + x;
add(3,4)
"
run e1


let e1 = fromString "
fun ignore (Bool x):Nil = ();
ignore(true)
"
run e1


let e2 = fromString "
var y = 4;
fun f(Int x):Int = if x = 0 then 0 else x + f(x-1);
f({var y = 5; y + 1})
"
run e2


let e2 = fromString "
var a = 5;
var b = a + 1;
fun f(Int x):Int = x + b;
f(3)
"
run e2

let e2 = fromString "
var a = 4;
var b = 7;
fun swap(Int x, Int y):(Int, Int) = (y, x);
var p = swap(a,b);
var p1 = swap(p);
p1
"
run e2

let e2 = fromString "
fun f0(): Int = 5 ;
fun f1(Int x): Bool = 0 < x ;
fun f2(Int x, Bool b): Int = if b then {x + 1} else x ;
fun f3(Int n, Int x, Int y): Int = n * x + y ;
f3(f0(), f2(3,true), 10)
"
run e2

let e3 = fromString "
{fun f(Int x):Int = x + 1 ;
 fun g(Int x):Int = 2 * x ;
 f(2) + g(3)
}
"
run e3


let e4 = fromString "
fun fact (Int x):Int = {
    if x = 0 then 1
    else x * fact(x - 1)
};
fact(n)
"
PlcInterp.eval e4 [("n", IntV 6)]

let e5 = fromString "
var x = true;
if x then 10 else 20
"
run e5

let e6 = fromString "
var in = 5;
fun f(Int x):Int = if in < 0 then -1 else if in = 0 then 0 else 1;
f(in)
"
run e6

let e7 = fromString "
var let = 100; var end = let + 1; end
"
run e7


let ex1 = fromString "{var x = 5+7; x}"
run ex1

let ex4 = fromString "
var y = 11 ;
fun f(Int x):Int = x + y ;
var y = 22 ;
f(y)
"
run ex4


let ex5 = fromString "
fun inc(Int x):Int = x + 1;
fun fib(Int n):Int = {
  fun ge2(Int x):Bool = 1 < x ;
  if ge2(n) then fib(n-1) + fib(n-2) else 1
} ;
fib(25)
"
run ex5

let ex6 = fromString "
fun f(Int x):Int = {
  fun g(Int y,Int z):Int = z * y ;
  g(x,x) + 1
}
;
f(3)
"
run ex6
