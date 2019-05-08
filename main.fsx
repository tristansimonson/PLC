(*
  Make sure you regenerate the Parser and Lexer
  every time you modify PlcLexer.fsl or PlcParser.fsy
*)

(*
  cd /Users/tejasmallela/Desktop/PLC-master
  mono bin/fslex.exe --unicode PlcLexer.fsl
  mono bin/fsyacc.exe --module PlcParser PlcParser.fsy

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
#load "C:\Users\\trist\Desktop\project\Plc.fs"
#load "C:\Users\\trist\Desktop\project\Test.fs"
#load "C:\Users\\trist\Desktop\project\TestAux.fs"

#r "/Users/tejasmallela/Desktop/PLC-master/bin/FsLexYacc.Runtime.dll"
#load "/Users/tejasmallela/Desktop/PLC-master/Environ.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/Absyn.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/PlcParserAux.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/PlcParser.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/PlcLexer.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/Parse.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/PlcInterp.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/PlcChecker.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/Plc.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/Test.fs"
#load "/Users/tejasmallela/Desktop/PLC-master/TestAux.fs"

open Absyn
let fromString = Parse.fromString // string parser function
let run e = printfn "\nResult is  %s\n" (Plc.run e)   // execution function

TestAux.testAll Test.cases

let typeCheck e = PlcChecker.teval e []
let interp e = PlcInterp.eval e []

(* Examples in concrete syntax *)

let e1 = fromString "
var e = ([Bool] []);
true::false::e
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun f(Int x, Int y, Int z) = x - y * z ; f(5,4,2)
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun f (Int x) = fn (Int y) => x+y end; f(3)(4)
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
hd (1 :: 2 :: ([Int][]))
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var f = fn (Int x) => x end; f(10)
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun rec map ((Int -> Int) f) : ([Int] -> [Int]) =
fn ([Int] l) =>
if ise(l) then l else f(hd(l)) :: map(f)(tl(l))
end ;
map (fn (Int x) => 2*x end) (10::20::30::([Int] []))
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun rec f(Int x) : Int =
match x with
| 2 -> 2
| _ -> 1
end;
f(2)
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var x = 2;
fn (Int x) => -x end
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var a = (3,4);
a[1] < a[2]
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var x = 2;
var y = 1;
y; x
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var x = 5; 1 + {var tmp = 9; x + x}
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun rec f(Int x) : Int = x; f(1)
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
tl([Bool][])
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun rec mem(Int x, [Int] l): Bool =
if ise(l) then false
else if x = hd(l) then true else mem(x, tl(l));
mem(2, 1::2::([Int] []))
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
fun inc (Int x) = x + 1;
fun add (Int x, Int y) = x + y;
fun cadd (Int x) = fn (Int y) => x + y end;
var y = add(3, inc(4));
var x = cadd(3)(7-y);
var z = x * 3;
fun rec fac (Int n) : Int =
match n with
| 0 -> 1
| 1 -> 1
| _ -> n * fac(n - 1)
end
;
print x; print y;
x :: y :: z :: fac(z) :: ([Int] [])
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var E = ([Int] []);
fun reverse ([Int] l) = {
fun rec rev ([Int] l1, [Int] l2): [Int] =
if ise(l1) then
l2
else
rev(tl(l1), hd(l1)::l2);
rev(l, E)
};
reverse (1::2::3::E)
"
typeCheck e1
interp e1
run e1

let e1 = fromString "
var E = ([Int] []);
fun reverse ([Int] s) = {
fun rec rev ([Int] s1, [Int] s2): [Int] =
match s1 with
| E -> s2
| _ -> {
var h = hd(s1);
var t = tl(s1);
rev(t, h::s2)
}
end
;
rev(s, E)
};
reverse (1::2::3::E)
"
typeCheck e1
interp e1
run e1

let e2 = fromString "
var a = 5;
var b = a + 1;
fun rec f(Int x) : Int = x + b;
f(3)
"
typeCheck e1
interp e1
run e1

let ex4 = fromString "
var y = 11 ;
fun rec f(Int x) : Int = x + y ;
var y = 22 ;
f(y)
"
typeCheck e1
interp e1
run e1

let ex1 = fromString "
fun twice (Int -> Int f) = fn (Int x) => f(f(x)) end ;
fun rec map (Int -> Int f) : ([Int] -> [Int]) =
fn ([Int] s) =>
if ise(s) then s else f(hd(s)) :: map(f)(tl(s))
end ;
fun square (Int x) = x * x ;
fun inc (Int x) = x + 1 ;
var E = ([Int] []) ;
var s1 = map (fn (Int x) => 2*x end) (10::20::30::E) ;
var s2 = map (twice(inc)) (s1) ;
(s1, s2)
"
typeCheck e1
interp e1
run e1
