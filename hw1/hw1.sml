fun to_day(d: int * int * int) =
  let
    val m = (#2 d + 9) mod 12
    val y = #1 d - m div 10
  in
    365*y + y div 4 - y div 100 + y div 400 + (m*306 + 5) div 10 + (#3 d - 1 )
  end

fun is_older(a: int * int * int, b:int * int * int) =
  to_day(a) < to_day(b)

fun number_in_month(l: (int * int * int) list, m: int) =
  if null l
  then 0
  else (if (#2 (hd l)) = m then 1 else 0) + number_in_month(tl l, m)