(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.(a) *)
fun all_except_option (str, []) =
    NONE
  | all_except_option (str, str'::str_list) = 
    case same_string (str, str') of
	true => SOME str_list
      | false => case all_except_option (str, str_list) of
		     NONE => NONE
		   | SOME str_list' => SOME (str'::str_list') (* need () here!! *)

(* 1.(b) *)
fun get_substitutions1 ([], str) =
    []
  | get_substitutions1 (str_list::str_list_list, str) =
    case all_except_option (str, str_list) of
	NONE => get_substitutions1 (str_list_list, str)
      | SOME str_list' => str_list' @ get_substitutions1 (str_list_list, str) 

(* 1.(c) *)
fun get_substitutions2 (str_list_list, str) =
    let fun func_helper ([], ans_list) = 
	    ans_list
	  | func_helper (str_list::str_list_list', ans_list) =
	    case all_except_option (str, str_list) of
		 NONE => func_helper (str_list_list', ans_list)
	       | SOME ans_list' => func_helper (str_list_list', ans_list' @ ans_list)
    in
	func_helper (str_list_list, [])
    end

(* 1.(d) *)
fun similar_names (str_list_list, {first, last:string, middle:string}) = (* the type of middle or last is unknow *) 
    let fun func_helper ([], ans_list) =
	    ans_list
	  | func_helper (str::str_list', ans_list) =
	    func_helper (str_list', {first = str, middle = middle, last = last}::ans_list)
    in
	func_helper(first::get_substitutions2(str_list_list, first), [])
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
(* 2.(a) *)
fun card_color cd = (* suit*'a -> color *)
    case cd of 
	(Clubs, _) => Black
     | (Spades,_) => Black
     | _ => Red

(* 2.(b) *)
fun card_value cd = 
    case cd of
	(_, Num num) => num
     |  (_, Ace) => 11
     | (_, _) => 10

(* 2.(c) *)
fun remove_card (cd_list, cd, e) =
    case cd_list of
	[] => raise e
	   | card::cd_list' =>  
	     case (card=cd) of
		 true => cd_list'
	       | false => card::remove_card(cd_list', cd, e)

(* 2.(d) *)
fun all_same_color [] =
    true
  | all_same_color [cd1] = true
  | all_same_color [cd1, cd2] = 
    card_color cd1 = card_color cd2
  | all_same_color (cd1::cd2::cd_list) =
    case (card_color cd1 = card_color cd2) of
	false => false
     | true => all_same_color(cd2::cd_list)

(* 2.(e) *)
fun sum_cards cd_list = 
    let fun func_helper (card_list, sum) = 
	    case card_list of
		[] => sum
	      | card::card_list' => func_helper (card_list', sum + card_value(card)) 
    in
	func_helper (cd_list, 0)
    end

(* 2.(f) *)
fun score (cd_list, goal) = 
    let
	val sum = sum_cards cd_list
	val pre_score = 
	    if ( sum > goal ) 
	    then 3 * (sum-goal)
	    else goal-sum
    in
	if (all_same_color cd_list)
	then pre_score div 2
	else pre_score
    end

(* 2.(g) *)
fun officiate (cd_list, mv_list, goal) =
    let 
	fun func_helper (cards, [], helds) =  (* warning: match nonexhaustive? *)
	    score (helds, goal)
	  | func_helper ([], Draw::moves, helds) =
	    score (helds, goal)
	  | func_helper (cd::cards, move::moves, helds) = 
	    case move of
		Discard card => func_helper (cd::cards, moves, remove_card (helds, card, IllegalMove))
	     | Draw =>
	       case ( (sum_cards helds) + (card_value cd) > goal ) of
		true => score( cd::helds, goal )
	      | false => func_helper (cards, moves, cd::helds)	    
    in
	func_helper(cd_list, mv_list, [])
    end

(* 3.(a) *)
fun ace_counter [] = 
    0
  | ace_counter (cd::cd_list) = 
    case cd of
	(_, Ace) => 1 + ace_counter cd_list
      | _ => ace_counter cd_list

fun score_challenge (cd_list, goal) = 
    let
	val sum = sum_cards cd_list
	fun get_pre_score sum =
	    if (sum > goal)
	    then 3 * (sum - goal)
	    else goal -sum
	fun get_min_pre_score (current_min_pre_score, ace_num) =
	    case (ace_num = 0) of
		true => current_min_pre_score
	      | false =>
	       if (get_pre_score (sum - ace_num * 10) < current_min_pre_score)
	       then get_min_pre_score (get_pre_score (sum - ace_num * 10), ace_num-1)
	       else get_min_pre_score (current_min_pre_score, ace_num-1)
	val pre_score =  get_min_pre_score (get_pre_score sum, ace_counter cd_list)
    in
	if (all_same_color cd_list)
	then pre_score div 2
	else pre_score
    end

fun officiate_challenge (cd_list, mv_list, goal) =
    let 
	fun func_helper (cards, [], helds) =  (* warning: match nonexhaustive? *)
	    score_challenge (helds, goal)
	  | func_helper ([], Draw::moves, helds) =
	    score_challenge (helds, goal)
	  | func_helper (cd::cards, move::moves, helds) = 
	    case move of
		Discard card => func_helper (cd::cards, moves, remove_card (helds, card, IllegalMove))
	     | Draw =>
	       case ( (sum_cards helds) + (card_value cd) - 10 * (ace_counter (cd::helds) ) > goal ) of
		true => score_challenge( cd::helds, goal )
	      | false => func_helper (cards, moves, cd::helds)	    
    in
	func_helper(cd_list, mv_list, [])
    end

(* 3.(b) *) 
fun try(cd_list:card list, held_list, goal, IllegalMove) = 
    case cd_list of
	[] => raise IllegalMove
      | cd::card_list => 
	if ( score(remove_card(held_list, cd, IllegalMove), goal) = 0 )
	then cd
	else try(card_list, held_list, goal, IllegalMove)
				    
fun has_card(cd_list, held_list, goal) = 
    case cd_list of
	[] => false
      | cd::card_list => 
	if ( score(remove_card(held_list, cd, IllegalMove), goal) = 0 )
	then true
	else has_card(card_list, held_list, goal)
   
fun careful_player ( cd_list, goal ) = 
    let fun game ([], held_list) =
	    []
	  | game (cd::card_list, held_list) = 
	    if (sum_cards held_list > goal)
	    then []
	    else 
		if (sum_cards held_list + 10 < goal)
		then Draw::game(card_list, cd::held_list)
		else 
		    if ( score(held_list, goal) = 0 )
		    then []
		    else 
			let 
			    val has_card = has_card(cd::held_list, cd::held_list, goal)
			in
			    if (has_card=true)
			    then [(Discard) (try(cd::held_list, cd::held_list, goal, IllegalMove))]
			    else []
			end
    in
	game(cd_list, [])
    end
