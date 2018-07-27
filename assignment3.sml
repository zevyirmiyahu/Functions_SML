(* Coursera Programming Languages Part A, Homework 3 
   Author: Zev Yirmiyahu
   Email: zy@zevyirmiyahu.com
   Website: zevyirmiyahu.com
   GitHub: https://github.com/zevyirmiyahu
 *)

(* ABOUT: This is one of the assignments from a course. The main focus is on function closures *) 


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

(* Returns a list of strings whose first letter is capital *)
fun only_capitals xs =
    List.filter(fn s => Char.isUpper(String.sub(s, 0))) xs

(*    
(* Doesn't use Filter, must use Filter in assignment *)
fun only_capitals xs : string list =
    case xs of
	[] => []
      | x::xs' =>
	if Char.isUpper(String.sub(x, 0))
	then x::only_capitals xs'
	else only_capitals xs'
*)

			       
(* Returns the longest string in a list NOTE: In case of tie, returns first string in the tie*)
fun longest_string1 xs =
    foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" xs

	  
(* Returns the longest string in a list. NOTE: In case of tie, return SECOND string in the tie *)
fun longest_string2 xs =
    foldl (fn (x,y) => if String.size(x) > String.size(y) then x else if String.size(x) = String.size(y) then x else y ) "" xs

	  
(* passed a function that behaves like > if true then behaves exactly like longest_string1 *)
fun longest_string_helper f xs =
    List.foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else acc) "" xs
	  

(* Has same behavior as longest_string1 *)
fun longest_string3 xs = longest_string1 xs 

					 
(* Has same behavior as longest_string2 *)
fun longest_string4 xs = longest_string2 xs

					 
(* Returns the longest string in a list that has the first letter capilalized *)
fun longest_capitalized xs =
    (longest_string1 o only_capitals) xs

				      
(* Reverses a strings using composition of functions and problem required the use of 3 library functions *)
fun rev_string s =
    (String.implode o List.rev o String.explode) s



(* Returns the first element in a list that satisfy the function f *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' =>
	case f(x) of
	    NONE => first_answer f xs'
	  | SOME a => a
	 
						 
(* Returns a list of all elements that satisfy the function f *)
fun all_answers f xs =
    let fun aux (xs, acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' =>
		case f(x) of
		    SOME a => aux(xs', a@acc) 
		  | NONE => NONE
    in
	aux(xs, [])
    end

	    
(* Takes a pattern and determines how many wildcards are present *)
fun count_wildcards pat =
   g (fn x => 1) ( fn x => 0) pat



(* Returns number of wildcard patterns PLUS sum of string lengths of all strings found *)
fun count_wild_and_variable_lengths pat =
    g (fn x => 1) (fn x => String.size(x)) pat


(* Input is a string and a pair, function returns number of times string appears as a variable in pattern *)
fun count_some_var input =
    case input of
	(s, pat) =>
	g (fn x => 0) (fn x => if String.isSubstring x s then 1 else 0) pat


(* Takes a pattern and returns true only if all the variables are distinct from one another *)
fun check_pat p =
    let
	fun var p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (v,vs) => vs @ var v) [] ps
	      | ConstructorP(_,p) => var p
	      | _ => []

	fun string_repeat xs =
	    case xs of
		[] => true
	      | x::xs' => if List.exists (fn y => y = x) xs' then false else string_repeat xs'
    in
	string_repeat(var(p))
    end

	
(* Takes a valu and pattern and returns list option, NONE if no match and SOME list if match. NOTE: Note that if the value matches but the pattern has no patterns of the form Variable s then the result is SOME [] *)	 
fun match (v : valu, p : pattern) =
    case p of
        Variable x => SOME [(x, v)]
      | UnitP =>
        (case v of
             Unit => SOME []
           | _ => NONE)
      | Wildcard => SOME []
      | ConstP k =>
        (case v of
             Const(v) => if k = v then SOME [] else NONE
           | _ => NONE)
      | TupleP ps =>
        (case v of
             Tuple(vs) => if List.length vs = List.length ps
                          then all_answers match (ListPair.zip(vs, ps))
                          else NONE
           | _ => NONE)
      | ConstructorP(s1,p') =>
        (case v of
             Constructor(s2,v') =>
             if s1 = s2 then match(v',p') else NONE
           | _ => NONE)

	    
(* Takes a valu and a list of patterns and returns a (string * valu ) list option. Returns NONE if no pattern in the list matches OR returns SOME list of bindings for the first pattern in the list that matches. *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE

			   
