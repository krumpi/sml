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

fun all_same_color(cs)=
  case cs of
    [] => true
  | x :: [] => true
  | x :: y :: xs => (card_color(x)= card_color(y)) andalso all_same_color(y :: xs)

fun sum_cards(cs)=
  let
    fun sum_cards_internal(acc, l)=
      case l of
        [] => acc
      | x::xs => sum_cards_internal(card_value(x) + acc, xs)
  in
    sum_cards_internal(0, cs)
  end

fun score(hc, g)=
  let
    val sum = sum_cards(hc)
    val ps = if sum > g then 3 * (sum - g) else g - sum
  in
    if all_same_color(hc) then ps div 2 else ps
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

val t41=card_color((Clubs, Num 1))=Black
val t42=card_color((Spades, Num 1))=Black
val t43=card_color((Diamonds, Num 1))=Red
val t44=card_color((Hearts, Num 1))=Red

val t51=remove_card([(Clubs, Num 1), (Spades, Num 1)], (Clubs, Num 1), IllegalMove)=[(Spades, Num 1)]
val t52=remove_card([(Clubs, Num 1), (Spades, Num 1)], (Spades, Num 1), IllegalMove)=[(Clubs, Num 1)]
val t53=remove_card([(Clubs, Num 1), (Spades, Num 1)], (Spades, Num 4), IllegalMove)=[] handle IllegalMove => true

val t61=all_same_color([])
val t62=all_same_color([(Clubs, Num 1)])
val t63=all_same_color([(Clubs, Num 1), (Spades, Num 1)])
val t64=not (all_same_color([(Clubs, Num 1), (Hearts, Num 1)]))

val t71=sum_cards([])=0
val t72=sum_cards([(Clubs, Num 1)])=1
val t73=sum_cards([(Clubs, Num 1), (Spades, Num 1)])=2
val t74=sum_cards([(Clubs, Num 1), (Hearts, Jack)])=11

val t81=score([],20)=10
val t82=score([(Spades, Num 1)],20)=9

val cards1 = [(Clubs,Jack),(Spades,Num(8))]
val cards2 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val cards3 = [(Clubs,Ace),(Diamonds,King)]

val strings = ["one","two","three","four", "two", "six"]
val names1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
val names2 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]

val test11 = all_except_option ("five", strings)=NONE
val test12 = all_except_option ("two", strings)=SOME(["one","three","four", "two", "six"])

val test21 = get_substitutions1(names1, "Fred")=["Fredrick","Freddie","F"]
val test22 = get_substitutions1(names2, "Jeff")=["Jeffrey","Geoff","Jeffrey"]   
val test23 = get_substitutions2(names2, "Maris")=[]

val test31 = get_substitutions2(names1, "Fred")=["Fredrick","Freddie","F"]
val test32 = get_substitutions2(names2, "Jeff")=["Jeffrey","Geoff","Jeffrey"]   
val test33 = get_substitutions2(names2, "Maris")=[]

val test42 = similar_names(names2, {first="Jeff", middle="W", last="Smith"} ) = 
         [{first="Jeff", last="Smith", middle="W"},
              {first="Jeffrey", last="Smith", middle="W"},
              {first="Geoff", last="Smith", middle="W"},
              {first="Jeffrey", last="Smith", middle="W"}]

val test43 = similar_names(names1, {first="Jeff", middle="W", last="Smith"} ) = [{first="Jeff", middle="W", last="Smith"}]

val testA1 = card_color((Clubs,Jack))=Black
val testA2 = card_color((Spades,Jack))=Black
val testA3 = card_color((Diamonds,Ace))=Red
val testA4 = card_color((Hearts,Ace))=Red

val testB1 = card_value((Clubs,Jack))=10
val testB2 = card_value((Clubs,Queen))=10
val testB3 = card_value((Clubs,King))=10
val testB4 = card_value((Clubs,Ace))=11
val testB5 = card_value((Clubs,Num(2)))=2
val testB6 = card_value((Clubs,Num(3)))=3
val testB7 = card_value((Clubs,Num(10)))=10

val testC1 = remove_card(cards1,(Clubs,Jack),IllegalMove)=[(Spades,Num(8))]
val testC2 = remove_card(cards2,(Spades,Ace),IllegalMove)=[(Clubs,Ace),(Clubs,Ace),(Spades,Ace)]
val testC3 = remove_card(cards2,(Clubs,Ace),IllegalMove)=[(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val testC4 = remove_card(cards1,(Spades,Num(8)),IllegalMove)=[(Clubs,Jack)]
val testC5 = (remove_card(cards2,(Spades,Num(8)),IllegalMove) handle IllegalMove => []) = []

val testD1 = all_same_color(cards1)=true
val testD2 = all_same_color(cards2)=true
val testD3 = all_same_color([(Clubs,Jack),(Spades,Num(8)),(Hearts,King)])=false
val testD4 = all_same_color([(Clubs,Jack),(Hearts,King),(Spades,Num(8))])=false
val testD5 = all_same_color([(Hearts,King),(Clubs,Jack),(Spades,Num(8))])=false
val testD6 = all_same_color(cards3)=false

val testE1 = sum_cards(cards1)=18
val testE2 = sum_cards(cards2)=44
val testE3 = sum_cards(cards3)=21

val testF1 = score(cards3,21)=0
val testF2 = score(cards3,25)=4
val testF3 = score(cards3,17)=12

val testF4 = score(cards2,44)=0
val testF5 = score(cards2,48)=2
val testF6 = score(cards2,40)=6

val testF7 = score([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)=3

(*val testG0 = ( officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)] ,42) handle IllegalMove => 9999 ) = 9999

val testG1 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],42)=3

val testG2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],30)=4
val testG3 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],22)=16
val testG4 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],100)=28
val testG5 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],44)=0

val testG6 = officiate([(Diamonds,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],30)=9
val testG7 = officiate([(Clubs,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],22)=33
val testG8 = officiate([(Clubs,Ace),(Spades,Ace),(Diamonds,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],100)=56
val testG9 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw,Draw,Draw,Draw],44)=0

val testG10 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],30)=8
val testG11 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],22)=0
val testG12 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],11)=33

val testG13 = officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=33
val testG14 = officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0
val testG15 = officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=8

val testG16 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=16
val testG17 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0
val testG18 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=4

val testG19 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],11)=30
val testG20 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],22)=0
val testG21 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],30)=4*)

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