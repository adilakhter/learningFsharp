(* The double pipeline (||>) operator 
 * ==================================
 * Consider a tuple (a,b) 
 * We want to check whether fst equal to snd. 
 * We could simply deconstruct a and b 
 * and then check equality as follows. 
 *)

(*
 * : eq : 'a -> 'b -> bool when 'a: equality
 *)
let eq a b = 
  a = b

let a,b = (1,1)   // val a : int = 1  val b: int = 2
a |> eq b // val it : bool = true 

(* 
 * But following is a concise way to achieve similar result by 
 * using ||> operator.
 *)
(1,1) ||> eq // val it:bool = true
(1,2) ||> eq // val it:bool = false
