fun is_older (date1:int*int*int, date2:int*int*int) = (* Returns true if the first date comes befroe the second, false otherwise. *)
    if (#1 date1 < #1 date2)
    then true
    else if (#1 date1 > #1 date2)
	 then false
	 else if (#2 date1 < #2 date2)
	      then true
	      else if (#2 date1 > #2 date2)
		   then false
		   else if (#3 date1 < #3 date2)
			then true
			else false;

fun number_in_month (list_of_dates:(int*int*int) list, month:int) = (* Returns how many dates in the list are in the given month. *)
    if (null list_of_dates)
    then 0
    else if (#2 (hd list_of_dates) = month)
	 then 1+number_in_month(tl list_of_dates, month)
	 else number_in_month(tl list_of_dates, month);

fun number_in_months(list_of_dates:(int * int * int) list, list_of_months:int list) = (* Returns how many dates in the list are in the given months. *)
    if (null list_of_months)
    then 0
    else number_in_month(list_of_dates, hd list_of_months) + number_in_months(list_of_dates, tl list_of_months);

fun dates_in_month(list_of_dates:(int*int*int) list, month:int) = (* Returns a list of dates that are in the givn month. *)
    if (null list_of_dates)
    then []
    else if (#2 (hd list_of_dates) = month)
	 then (hd list_of_dates)::dates_in_month(tl list_of_dates, month)
	 else dates_in_month(tl list_of_dates, month);

fun dates_in_months(list_of_dates:(int * int * int) list, list_of_months:int list) = (* Returns a list of dates that are in the givn months.*)
    if (null list_of_months)
    then []
    else dates_in_month(list_of_dates, hd list_of_months) @ dates_in_months(list_of_dates, tl list_of_months);

fun get_nth(list_of_strings:string list, nth:int) = (* Returns the nth string *)
    if (nth<=1)
    then hd list_of_strings
    else get_nth(tl list_of_strings, nth-1);

fun date_to_string( date:int * int * int ) = (* Returns a string representing the date. *)
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
	get_nth(months, #2 date)^" "^(Int.toString(#3 date))^", "^(Int.toString(#1 date))
    end;

fun number_before_reaching_sum(sum:int, list_of_num:int list) = (* Returns the number before the first element add to less than sum. *)
    if ( hd list_of_num >= sum )
    then 0
    else 1+number_before_reaching_sum(sum - hd list_of_num, tl list_of_num);
 
fun what_month(day:int) = (* Returns what month that day is in *)
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
	1 + number_before_reaching_sum( day, days_in_months )
    end;

fun month_range(day1:int, day2:int) = (* Returns a month list representing the month from day1 to day2. *)
    if ( day1 > day2 )
    then []
    else what_month(day1)::month_range(day1+1, day2);

fun oldest(list_of_dates: (int * int * int) list) = (* Returns NONE or SOME date d is the oldest date in the list. *)
    if null list_of_dates
    then NONE
    else 
	let val tl_ans = oldest(tl list_of_dates)
	in
	    if isSome tl_ans
	       andalso is_older(valOf tl_ans, hd list_of_dates)
	    then tl_ans
	    else SOME (hd list_of_dates)
	end;

fun is_in_the_list(month:int, list_of_month:int list) = (* Returns true if month is in the list, false otherwise. *)
    if ( null list_of_month )
    then false
    else if ( month = hd list_of_month )
    then true
    else is_in_the_list(month, tl list_of_month);

fun remove_repeated_month(list_of_month:int list) = (* Returns a month list in which no month repeated. *)
    if null list_of_month
    then []
    else if is_in_the_list(hd list_of_month, tl list_of_month)
    then remove_repeated_month(tl list_of_month)
    else (hd list_of_month)::remove_repeated_month(tl list_of_month);

fun number_in_months_challenge(list_of_dates:(int * int * int) list, list_of_months:int list) = (* Returns how many dates in the list are in the given months. *)
    number_in_months(list_of_dates, remove_repeated_month(list_of_months));

fun dates_in_months_challenge(list_of_dates:(int * int * int) list, list_of_months:int list) = (* Returns a list of dates that are in the givn months.*)
    dates_in_months(list_of_dates, remove_repeated_month(list_of_months));

fun get_days_in_month(year:int) = (* Returns a list representing how many days in month from Jan to Dec *)
    if ( year mod 4 <> 0 )
    then [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    else if ( year mod 100 = 0 andalso year mod 400 <> 0 )
    then [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    else [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

fun get_days_in_nth_month(days_in_month:int list, nth:int) = (* Returns the nth int. *)
    if (nth<=1)
    then hd days_in_month
    else get_days_in_nth_month(tl days_in_month, nth-1);

fun reasonable_date(date:int*int*int) = (*Returns true if it is a reasonable date. *)
    if ( #1 date <= 0 )
    then false
    else if #2 date < 1 orelse #2 date > 12
    then false
    else let val days_in_month = get_days_in_month(#1 date)
	 in
	     if ( #3 date >= 1 andalso #3 date <= get_days_in_nth_month(days_in_month, #2 date) )
	     then true
	     else false
	 end;
