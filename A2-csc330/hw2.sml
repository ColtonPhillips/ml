(* There will be 11 SML functions, 4 name substitutions, 7 related to solitare *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)


fun all_except_option(s:string, string_list: string list ) : string list option=
let 
  fun helper(s:string, s_l: string list, count:int) : int  = 
     case s_l of
          [] => ~1
        | x::xs => if (same_string(s,x)) then
                     count 
                   else
                      helper(s,xs, count + 1)
                      
        
  val result = helper(s,string_list, 0)                   
  fun helper2 (s_l:string list , count:int) : string list = 
    case s_l of 
         [] => []
       | x::xs => if (count = 0) then
                 xs
               else
                 [x] @ helper2(xs,count-1)
in
  if (result = ~1) then NONE else SOME (helper2(string_list,result))
end

fun get_substitutions1(s_l_l: string list list, s:string) :string list= 
  []
fun get_substitutions2(s_l_l: string list list, s:string) :string list= 
  []

type Person = {first:string,middle:string,last:string}  
fun similar_names(s_l_l: string list list, person:Person ) : Person list = 
  []

(*
fun get_substitutions1(s_l_l: string list list, s:string) :string list= 
let

  fun remove_string(s_l: string list, s: string) : string list = 
    case s_l of
         [] => []
       | a::as => if (same_string(a,s)) then
                    remove_string(as,s)
                  else
                    [a] @ remove_string(as,s)

  fun helper(s_l_l:string list list, s:string) :string list = 
    case s_l_l of
         [] => []
       | x::xs => case x of
                       [] => []
                     | y::ys => if (same_string(s,y)) then 
                                  remove_string(x,s)
                                else
                                  helper(


        

in
end


get_substitutions1([["Fred","Fredrick"],
                        ["Elizabeth","Betty"],
                        ["Freddie", "Fred", "F"]], "Fred");

get_substitutions1([["Fred","Fredrick"],
                        ["Jeff","Jeffrey"],
                        ["Geoff","Jeff","Jeffrey"]], "Jeff");                     


fun smee(s_l_l:string list list, s:string):string list=
let 

  fun remove_string(s_l: string list, s: string) : string list = 
    case s_l of
         [] => []
       | a::as => if (same_string(a,s)) then
                    remove_string(as,s)
                  else
                    [a] @ remove_string(as,s)
  
  fun help(s_l_l:string list list, s:string, built:list) : string list = 
    case s_l_l of
         [] => []
       | x::xs => case x of
                       [] => []
                     | y::ys => if (same_string(s,y)) then 
                                  help(xs, s, built @ remove_string(x,s))
                                else

in

end


                        *)
                        
(* Game  *) 
(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)

fun card_color(my_card:card) : color = 
  case my_card of
       (Clubs,_) => Black
     | (Diamonds,_) => Red
     | (Hearts,_) => Red
     | (Spades,_) => Black;

fun card_value(my_card:card) : int =
  case my_card of
       (_,Ace) => 11
     | (_,Num(x)) =>x 
     | (_,_) => 10

fun remove_card(cs:card list, c:card, e:exn) : card list = 
  []
  (*
fun remove_card(cs:card list, c:card, e:exn) : card list = 
let fun has_card(c_l:card list, _c:card) : bool =  
        case c_l of
             [] => false
             y::ys => if (y = _c) then true else has_card(ys,_c)
in
  if (has_card(cs,c)) then
  (case cs of
       [] => []
     | x::xs => if (c = x) then
                  xs
                else  
                  [x] @ remove_card(xs,c,e))
   else
     raise e
end
*)

     (*
fun remove_card(cs:card list, c:card, e:exception IllegalMove) : bool = 
  false;
  (*
  [(Clubs,Ace),(Diamonds,King)];*)
  (*
  case cs of
       [] => cs
     | x::xs => if (c = x) then
                  xs
                else
                  [x] @ remove_card(xs,c,e)  ;
                  *)
card_color((Clubs,Num(3)));
card_color((Diamonds,King));

card_value((Clubs,Num(2)));
card_value((Diamonds,Ace));
card_value((Hearts,Num(8)));

all_same_color([(Hearts,Ace),(Diamonds,Ace),(Hearts,Ace)]);
sum_cards([(Hearts,Ace),(Diamonds,Ace),(Hearts,Ace)]);
all_except_option("a", ["c","m","a","d"]);
all_except_option("i", ["c","m","a","d"]);
*)

fun all_same_color(cl:card list) : bool = 
  case cl of
       [] => true
     | x::xs => case xs of
                     [] => true
                   | y::ys => if (card_color(x) = card_color(y)) then 
                               all_same_color(xs)
                             else
                               false;

fun sum_cards(cl:card list) : int = 
let 
  fun local_help(cl:card list) : int = 
  case cl of
      [] => 0 
     | x::xs => card_value(x) + local_help(xs)
  
in case cl of 
        [] => 0
      |  _ => local_help(cl)
end;

fun score(cl:card list, goal:int): int = 
  1

fun officiate(cl:card list, ml:move list, goal:int): int =  
  4
