(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)

fun only_capitals(s_l:string list) : string list = 
let
  fun f (s) : bool = Char.isUpper(String.sub(s,0))
in
  List.filter(f)s_l 
end

fun longest_string1(s_l:string list) : string = 
let
  fun longer_string(s1:string,s2:string): string = 
    if (size s1 > size s2) then s1 else s2
in
      List.foldl(longer_string) "" s_l
end

fun longest_string2(s_l:string list) : string = 
let
  fun longer_string(s1:string,s2:string): string = 
    if (size s1 >= size s2) then s1 else s2
in
      List.foldl(longer_string) "" s_l
end

fun longest_string_helper(my_f:(int * int -> bool)) : string list -> string =
let
  fun jesus(x:string, y:string):string = 
    if (my_f(size x, size y)) then x else y
in
  fn (s_l:string list) => List.foldl(jesus) "" s_l
end

val longest_string3 = fn (s_l:string list) => 
  (
    longest_string_helper(
      (fn (x:int, y:int) => if (x > y) then true else
      false )
    ) s_l
  )


val longest_string4 = fn (s_l:string list) => 
  (
    longest_string_helper(
      (fn (x:int, y:int) => if (x >= y) then true else
      false )
    ) s_l
  )

fun longest_capitalized(s_l:string list) : string = 
  (longest_string1 o only_capitals) s_l

fun rev_string(s:string) : string = 
  (String.implode o List.rev o String.explode) s

fun first_answer(my_f:('a -> 'b option)) : 'a list -> 'b = 
let 
  fun help(a:'a list) : 'b = 
  let 
      val eh = (((List.map valOf)o(List.filter isSome)o(List.map my_f)) a)
  in
    if (length eh = 0) then raise NoAnswer else hd eh
  end
in
  help
end
(*jjj
fun all_answers(my_f:('a -> 'b list option)) : 'a list -> 'b list = 
let
  fun help(a:'a list) : 'b list = 
  let 
    val eh = (((List.map valOf)o(List.filter isSome)o(List.map my_f)) a)
  in 
    eh
  end
in
  help
end
*)
(*
;only_capitals(["a", "b", "C", "DE", "1", "one","Two"]);
longest_string1(["a","b", "def", "eee"]);
longest_string1([]);
longest_string2(["a","b", "def", "eee"]);

longest_string3(["a","b", "def", "eee"]);
longest_string4(["a","b", "def", "eee"]);


longest_capitalized(["aaaaaaaaaaaaaa", "B", "CDef"]);

rev_string("abcdefG");

first_answer(fn x => if (x mod 2) = 0 andalso x > 3 then SOME x else NONE) [1,2,3,4,5,6,7];
first_answer(fn x => if (x mod 2) = 0 andalso x > 3 then SOME x else NONE)
[0,2,3];
*)
