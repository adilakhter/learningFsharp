(*
 * UVa 10706. Number Sequence
 * 
 * http://uva.onlinejudge.org/external/107/10706.html
 *)
open System

let mutable numLength= 1
(*
 * :numbers:[int] -> length_each_num:[int]
 *)
let getNumLengths (l:int list) :int list = 
  (*
   * :num:Int -> numLength:Int 
   *)
  let computeLength (i:int):int =
    let rec computeLength' (i:int) (l:int) :int = 
      if ((i/(System.Math.Pow(10|>float,l|>float)|>int)) = 0) then 
        numLength <- l
        l 
      else 
        computeLength' i (l+1)
    computeLength' i numLength

  numLength <- 1 // setting numberLength <- 1 
  l
  |> List.map computeLength

(* Utility function to print a sequence 
 * :seq<'a> -> unit
 *)
let printSeq seq1 = Seq.iter (printf "%A ") seq1; printfn "" 


let cumulativeSumofLengths l = 
  let computeLengths list:int list =
    let rec computeNoOfElements' l acc = 
      match l with 
      | [] -> []
      | x::xs -> (x + acc)::(computeNoOfElements' xs (x + acc))
    computeNoOfElements' list 0  
  l 
  |> computeLengths  
  |> computeLengths 


(*
 * Performs a binary search on the cumulative sums of lengths. 
 * and return a tuple as (minBound, maxBound)
 * : arr: int[] -> n: int -> (int,int)
 *)
let getBounds a (n:int): int*int = 
  let binarySearch (arr:int[]) (n:int) : (int*int) =
    let rec binarySearch' (arr:int[]) (n:int)  (low:int) (high:int) = 
      if arr.[high] < n then 
         (high, System.Int32.MaxValue)
      else if high - low <= 1 then 
        if arr.[low] = n then
          (low, low)
        else  
          (low,high)
      else
        let mid = (high + low)/2
        match arr.[mid],n with 
        |  (a,b) when a <= b->  binarySearch' arr n  mid high
        |  _ ->  binarySearch' arr n  low mid
    binarySearch'  arr n 0 (arr.Length-1)
  binarySearch a n 
  
let numberAt a n = 
  let getDigitAtIndex (relLength:int) (index:int): int = 
    let rec getIntAtIndex' (relLength:int) (startNum:int)(endNum:int):int = 
      if startNum <= endNum then 
        let str = startNum.ToString()
        match  relLength  - str.Length with 
        | diff when diff <= 0 -> System.Int32.Parse(str.[relLength-1].ToString())
        | diff                ->  getIntAtIndex'  (relLength - str.Length)  (startNum+1) endNum
      
      else 
        -1
    getIntAtIndex' relLength 1 index  
  match  getBounds a n with 
  | 0,0  -> 1
  | l,h when h = System.Int32.MaxValue -> getDigitAtIndex (n-a.[l]) (l+1)  
  | l,h when l = h -> getDigitAtIndex (n-a.[l-1]) (l+1)
  | l,h            -> getDigitAtIndex (n-a.[l]) (l+2)


  
let solve_UVA10706 ()= 
  let arr = 
    [1..31267]
    |> getNumLengths 
    |> cumulativeSumofLengths 
    |> List.toArray

  let T = Console.ReadLine().Trim() |> int
  let mutable i:string = null

  for case_i = 1 to T do 
    i <- System.Console.ReadLine()
    if i <> null then 
      i
      |> Int32.Parse 
      |> numberAt arr
      |> printfn "%d"

// running script 
solve_UVA10706()

