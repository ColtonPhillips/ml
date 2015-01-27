(*  Assignment #1 *)

type DATE = {year:int, month:int, day: int}
exception InvalidParameter 

(* This file is where your solutions go *)


fun is_older(d1: DATE, d2: DATE): bool = 
  if (#year d1 > #year d2) then true else
        if ((#year d1 = #year d2) andalso (#month d1 > #month d2)) then true
        else 
          if ((#year d1 = #year d2) andalso (#month d1 = #month d2) andalso
          (#day d1 > #day d2)) then true 
          else false
            
fun number_in_month(dates:DATE list, month:int): int =
  case dates of
       [] => 0
     | x::xs => if (#month x = month) then 
       1 + number_in_month(xs,month) else
         number_in_month(xs,month)

fun number_in_months(dates:DATE list, months:int list): int = 
  case months of
       [] => 0
     | x::xs => number_in_month(dates,x) + number_in_months(dates,xs)

fun dates_in_month(dates:DATE list, month:int): DATE list = 
  if (length dates = 0) then []
  else if (#month (hd dates) = month) then
    [(hd dates)] @ dates_in_month(tl dates, month)
  else
    dates_in_month(tl dates, month)

fun dates_in_months(dates:DATE list, months:int list): DATE list = 
  if (length months = 0) then []
  else 
    dates_in_month(dates,hd months) @ dates_in_months(dates,tl months)
    
fun get_nth(strings:string list, n:int): string = 
  if (n=0) then raise InvalidParameter
  else if (n > length strings) then raise InvalidParameter
  else
    let fun nth(strings:string list, count:int): string = 
      if (count = 1) then
        hd strings
      else
        nth(tl strings, count-1)
    in
      nth(strings,n)
    end  
  
fun date_to_string (date:DATE): string = 
  let val month_index : string list = 
    ["January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"]

  in
    get_nth(month_index, #month date)
    ^ " " ^ Int.toString(#day date) ^ ", " ^ Int.toString(#year date)
  end

fun number_before_reaching_sum(sum:int, ints:int list): int = 
  let fun college_try(sum:int, ints:int list, count:int): int = 
    if ((hd ints) >= sum) then
      count
    else
      college_try(sum - (hd ints), tl ints, count+1)
  in
    college_try(sum,ints, 0)
  end


fun what_month(day_of_year:int): int = 
  let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum(day_of_year,days_in_months)
  end

fun month_range(doy1:int, doy2:int): int list = 
  if (doy1-1 = doy2) then
    []
  else
    [what_month(doy1)] @ month_range(doy1+1, doy2)
    
fun oldest(dates:DATE list): DATE option = 
  if null dates then NONE
  else
    let fun get_oldest(dates:DATE list, my_date:DATE): DATE option = 
    if (dates = []) then
        SOME ( my_date )
    else if (is_older(hd dates, my_date)) then
      get_oldest(tl dates, hd dates)
    else
      get_oldest(tl dates, my_date)
    in
      get_oldest(dates,hd dates)
    end
    

fun reasonable_date(date:DATE):bool = 
  false

    (* I could not repair the syntax error in time.*)
(*fun reasonable_date(date:DATE):bool = 
  let fun leap_year(year:int):bool = 
    if (((year mod 400) = 0) orelse ((not((year mod 100) = 0)) andalso ((year mod 4) = 0) )) then
      true
    else false
  in 
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
    if ( (#year date <= 0) orelse (#month date < 1) orelse (#month date > 12)
    orelse (#day date < 0) ) then
      false
    else if (#day date > List.nth(days_in_months, (#month date)-1)) then
      if ((#day date = 29) andalso leap_year(#year date)) then
        true
      else 
        false
    else
        true
    end
  end
  *)

(* Add your other functions here *)
(*
val a =  month_range(31,34);
val d1= {year = 2015, month = 1, day = 11};
val d2 = {year = 2014, month = 5, day = 11};
val d3 = {year = 2000, month = 11, day = 11};
val d4 = {year = 2014, month = 11, day = 11};
val d5 = {year = 2014, month = 11, day = 11};

val date_list = [d1, d2, d3, d4, d5]: DATE list;
val month_list = [11, 5]: int list;
number_in_month(date_list,1);
number_in_months(date_list,month_list);

val dik = dates_in_month(date_list,11);
val poo = dates_in_months(date_list, [1,11]);

val string_list = ["a", "b", "c", "d", "e"];
get_nth(string_list,3);

date_to_string(d1);
date_to_string(d2);

val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]; 
number_before_reaching_sum(32,days_in_months);
what_month(360);

month_range(31, 40);

oldest(date_list);
reasonable_date(d1);
val unreason = {year = 2000, month = 2, day = 29};
reasonable_date(unreason);
*)
