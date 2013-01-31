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

fun similar_names(sll, full_name:{first:string, middle:string, last:string})=
  let 
    val s = get_substitutions1(sll, #first full_name)
    fun acc_substitutions(acc, subs)=
      case subs of
          [] => acc
        | x :: xs => acc_substitutions({first=x, middle=(#middle full_name), last=(#last full_name)} :: acc, xs)
  in
    full_name :: acc_substitutions([], s)
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit, rank)=
  case suit of
      Spades => Black
    | Clubs => Black 
    | Diamonds => Red
    | Hearts => Red

fun card_value(suit, rank)=
  case rank of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num x => x

fun remove_card(cs, c, e)=
  let
    fun all_except_option_internal(acc, l) = 
    case l of
       [] => raise e
     | h :: xs => if h = c then acc @ xs else all_except_option_internal(h :: acc, xs)
  in
    all_except_option_internal([], cs)
  end

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

val t31=similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})=
  [{first="Fred", last="Smith", middle="W"},
  {first="F", last="Smith", middle="W"},
  {first="Freddie", last="Smith", middle="W"},
  {first="Fredrick", last="Smith", middle="W"}]

val t41=card_color((Clubs, 1))=Black
val t42=card_color((Spades, 1))=Black
val t43=card_color((Diamonds, 1))=Red
val t44=card_color((Hearts, 1))=Red

val t51=remove_card([(Clubs, 1), (Spades, 1)], (Clubs, 1), IllegalMove)=[(Spades, 1)]
val t52=remove_card([(Clubs, 1), (Spades, 1)], (Spades, 1), IllegalMove)=[(Clubs, 1)]
val t53=remove_card([(Clubs, 1), (Spades, 1)], (Spades, 4), IllegalMove)=[] handle IllegalMove => true

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