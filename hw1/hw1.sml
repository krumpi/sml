fun to_day(d: int * int * int) =
  let
    val m = (#2 d + 9) mod 12
    val y = #1 d - m div 10
  in
    365*y + y div 4 - y div 100 + y div 400 + (m*306 + 5) div 10 + (#3 d - 1 )
  end

(* Auxiliary function to determine if a date falls in a certain month *)
fun in_month(d: int * int * int, m: int) =
  if #2 d = m then 1 else 0

fun is_older(a: int * int * int, b:int * int * int) =
  to_day(a) < to_day(b)

fun number_in_month(l: (int * int * int) list, m: int) =
  if null l
  then 0
  else in_month(hd l, m) + number_in_month(tl l, m)

fun number_in_months(l: (int * int * int) list, m: int list) =
  if null m
  then 0
  else number_in_month(l, hd m) + number_in_months(l, tl m)

fun dates_in_month(l: (int * int * int) list, m: int) =
  if null l
  then []
  else if in_month(hd l, m) = 1 then hd l :: dates_in_month(tl l, m) else dates_in_month(tl l, m)

fun dates_in_months(l: (int * int * int) list, m: int list) =
  if null m
  then []
  else dates_in_month(l, hd m) @ dates_in_months(l, tl m)

fun get_nth(s: string list, n: int) =
  if n = 1
  then hd s
  else get_nth(tl s, n - 1)

fun date_to_string(d: int * int * int) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

fun number_before_reaching_sum(sum: int, l: int list) =
  let
    fun number_before_reaching_sum_internal(acc: int, p: int list) =
      if acc <= 0
      then length l - length p - 1
      else number_before_reaching_sum_internal(acc - hd p, tl p)
  in
    number_before_reaching_sum_internal(sum, l)
  end

fun what_month(day: int) =
  let
    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, month_days) + 1
  end

fun month_range(day1: int, day2: int) =
  let
    fun add_day(ans: int list, d: int) =
      if d < day1
      then ans
      else add_day(what_month(d) :: ans, d - 1)
  in
    add_day([], day2)
  end

fun oldest(dates: (int * int * int) list) = 
  let
    fun oldest_internal(ans: (int * int * int) option, d: (int * int * int) list) =
      if null d
      then ans
      else if is_older(hd d, valOf ans)
        then oldest_internal(SOME(hd d), tl d)
        else oldest_internal(ans, tl d)
  in
    if null dates
    then NONE
    else oldest_internal(SOME(hd dates), tl dates)
  end