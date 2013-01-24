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