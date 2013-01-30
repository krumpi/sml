(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, sl) =
  let
    fun all_except_option_internal(acc, l) = 
    case l of
       [] => NONE
     | h :: xs => if same_string(h, s) then SOME(acc @ xs) else all_except_option_internal(h :: acc, xs)
  in
    all_except_option_internal([], sl)
  end

fun get_substitutions1(sll, s) =
  case sll of
    [] => []
  | x::xs => case all_except_option(s, x) of
      NONE    => get_substitutions1(xs, s)
    | SOME(y) => y @ get_substitutions1(xs, s)

fun get_substitutions2(sll, s) =
  let
    fun get_substitutions2_internal(acc, l) = 
      case l of
        [] => acc
      | x::xs => case all_except_option(s, x) of
          NONE    => get_substitutions2_internal(acc, xs)
        | SOME(y) => get_substitutions2_internal(acc @ y, xs)

  in
    get_substitutions2_internal([], sll)
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)



(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

val t11=all_except_option("1", []) = NONE
val t12=all_except_option("1", ["1"]) = SOME([])
val t13=all_except_option("1", ["1", "2", "3"]) = SOME(["2", "3"])
val t14=all_except_option("1", ["2", "3"]) = NONE
val t15=all_except_option("Fred", ["Fred","Fredrick"]) = SOME(["Fredrick"])
val t16=all_except_option("Fred", ["Freddie","Fred", "F"]) = SOME(["Freddie", "F"])

val t21=get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie", "F"]
val t22=get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff", "Jeffrey"]

val t23=get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie", "F"]
val t24=get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff", "Jeffrey"]

(*fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end
*)