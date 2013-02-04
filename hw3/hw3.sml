(* Coursera Programming Languages, Homework 3, Provided Code *)

fun only_capitals(sl) = List.filter(fn x => Char.isUpper(String.sub(x, 0))) sl

fun longest_string1(sl) = List.foldl(fn (p, x) => if String.size(x) >= String.size(p) then x else p) "" sl

fun longest_string2(sl) = List.foldl(fn (p, x) => if String.size(x) > String.size(p) then x else p) "" sl

fun longest_string_helper(f)(sl) = List.foldl(fn (p, x) => if f(p, x) then x else p) "" sl

fun longest_string3(sl) = longest_string_helper(fn (p, x) => String.size(x) >= String.size(p))(sl)

fun longest_string4(sl) = longest_string_helper(fn (p, x) => String.size(x) > String.size(p))(sl)

fun longest_capitalized(sl) = (longest_string3 o only_capitals)(sl)

fun rev_string(s) = (String.implode o List.rev o String.explode) s

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* `HW3` Tests *)
(* Problem 1 *)
val names = ["Andy","Bernard","Cathy","Doug","Elaine","Fred","Greg","Harry","Isis","Jack","Karen","Laura","Mike","Nancy","Ophelia","Petunia","Quincy","Ray","Supercalifragilisticexpialidocious","Tom","Uma","Velma","Wilma","Xan","Yan","Zed"]

(* names2 is same as names, but longest word is lower case *)
val names2 = ["Andy","Bernard","Cathy","Doug","Elaine","Fred","Greg","Harry","Isis","Jack","Karen","Laura","Mike","Nancy","Ophelia","Petunia","Quincy","Ray","supercalifragilisticexpialidocious","Tom","Uma","Velma","Wilma","Xan","Yan","Zed"]

val names3 = ["andy","bernard","cathy","doug","elaine"]

val empty = []
val list1b = ["Andy","bernard","Cindy"]
val list1c = ["andy","bernard","Cindy"]
val list1d = ["andy"]

val test1a = only_capitals(empty)=[]
val test1b = only_capitals(list1b)=["Andy","Cindy"]
val test1c = only_capitals(list1c)=["Cindy"]
val test1d = only_capitals(list1d)=[]

(* Problem 2 *)
val list2a = ["Andy","Billy","Blair"]

val test2a = longest_string1(empty) = ""
val test2b = longest_string1(list1b) = "bernard"
val test2c = longest_string1(names) = "Supercalifragilisticexpialidocious"
val test2d = longest_string1(list2a) = "Billy"

(* Problem 3 *)
val test3a = longest_string2(empty) = ""
val test3b = longest_string2(list1b) = "bernard"
val test3c = longest_string2(names) = "Supercalifragilisticexpialidocious"
val test3d = longest_string2(list2a) = "Blair"

(* Problem 4 *)
val test4a = longest_string3(empty) = ""
val test4b = longest_string3(list1b) = "bernard"
val test4c = longest_string3(names) = "Supercalifragilisticexpialidocious"
val test4d = longest_string3(list2a) = "Billy"

val test4e = longest_string4(empty) = ""
val test4f = longest_string4(list1b) = "bernard"
val test4g = longest_string4(names) = "Supercalifragilisticexpialidocious"
val test4h = longest_string4(list2a) = "Blair"

(* Problem 5 *)
val test5a = longest_capitalized(names) = "Supercalifragilisticexpialidocious"
val test5b = longest_capitalized(names2) = "Bernard"
val test5c = longest_capitalized(names3) = ""
val test5d = longest_capitalized([]) = ""

(* Problem 6 *)
val test6a = rev_string("Supercalifragilisticexpialidocious")="suoicodilaipxecitsiligarfilacrepuS"
val test6b = rev_string("hello")="olleh"

fun is_even x =
    if x mod 2 = 0 then SOME([x]) else NONE

val first_answer_test1 = first_answer is_even [1, 2, 3, 4, 5, 6] = [2];
val first_answer_test2= (first_answer is_even [1, 3, 5] handle NoAnswer => [0]) = [0];
val first_answer_test3 = first_answer is_even [1, 3, 5, 6] = [6];
val first_answer_test4 = (first_answer is_even [] handle NoAnswer => [0]) = [0];

(* Tests for all_answers *)

(*val all_answers_test1 = all_answers is_even [1, 2, 3, 4, 5, 6] = NONE;
val all_answers_test2 = all_answers is_even [2, 4, 6] = SOME ([2, 4, 6]);
val all_answers_test3 = all_answers is_even [] = SOME ([]);*)