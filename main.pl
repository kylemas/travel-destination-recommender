% UBC Student Travel Destination Recommender
% CPSC 312 2019 Project #2
% Written by Sandy Co and Kyle Mas


% starting instructions for user
start :- write("Welcome to our UBC Student Travel Destination Recommender. note: all starting points are from YVR"),nl,
         sleep(0.2),
		 write("To get started, type ?- get(Countryrecs) and follow each step
		        to get a list of country recommendations.").

% main recommendation that the user queries from, according to the following rules:
% L is the list of countries the user has been to already
% N is the preferred continent the user wants to travel, 0 if the user has no preference
% W is the type of weather they want (tropical, cold, mild), 0 if the user has no preference
% B is the user's budget for the trip, 0 if the user has no budget
% T is 0 if the user wants to ignore tags, 1 if the user only wants countries
%   suggested that have shared tags as the countries they have been to
%   or is a tag if the user wants countries suggested to them if they have tag T
% C is the list of suggested countries.
% Note: This atom uses the setof atom to remove all duplicates.
% Note: This recommender is specific for UBC students, so we will be assuming that the starting point for travels is from YVR.

recommend(_,0,0,0,0,C) :-
	findall(Q, info(Q,_,_,_,_),C). % return all countries
recommend(L,N,W,B,T,C) :-
	setof(Q, Q^gather(L,N,W,B,T,Q),C).

% gather(L,W,B,N,T,C) is true if the country C meets all the requirements that the user selected
% Note: See the comments for each of the atoms that gather depends on
gather(L,N,W,B,T,C) :-
  call_check_continent(N,C),
	call_check_weather(W,C),
	call_check_tags(T,L,C),
	call_check_budget(B,C),
	not_in(L,C). % do not return countries already visited

% call_check_continent(N,C) is true if N equals 0, or if
% the country C is in continent N
call_check_continent(0,_).
call_check_continent(N,C) :-
	check_continent(N,C).

% call_check_weather(W,C) is true if W equals 0, or if W:
% equals 1 and the weather for country C is tropical
% equals 2 and the weather for country C is cold
% equals 3 and the weather for country C is warm
call_check_weather(0,_).
call_check_weather(W,C) :-
  check_weather(W,C).

% call_check_tags(T,L,C) is true if T is 0, if T is 1 and the
% the country C has at least one tag that's the same as a tag that a country in L has
% or if T is a tag and the country C has a tag T
call_check_tags(0,_,_).
call_check_tags(1,L,C) :-
  check_tags(L,C).
call_check_tags(T,_,C) :-
  find_countries(T,C).

% call_check_budget(B,C) is true if B equals 0, or if the
% country C is in within budget B
call_check_budget(0,_).
call_check_budget(B,C) :-
  check_budget(B,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% to collect user input (input from a line):
get(Countryrecs) :-
	write("Enter list of countries you have been to (ex. [canada, japan, philippines]): "),
	read(L),
  write("Enter 0 = get all continents; 'abcd' to search for specific continent: "),
	read(N),
	write("Enter 0 = if you have no weather preference; 1 = tropical; 2 = Cold, Winter weather; 3 = mild, cloudy or sunny weather: "),
	read(W),
	write("Enter 0 = get all countries regardless of budget; '>=i', '<=i', or
	'=i' where i is the maximum budget between 1-4 to get specific countries that are less than or equal to your. 1 = $; 2 = $$; 3 = $$$; 4 = $$$$"),
	read(B),
  write("Enter 0 = ignore tags; 1 = count country list as tags;
	'tag' to search for tagged courses: "),
	read(T),
  	recommend(L,N,W,B,T,Countryrecs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% not_in(L,C) is true if C is not in list L
not_in([],C).
not_in([H|T],C) :-
	dif(H,C),
	not_in(T,C).

% find_countries(T,C) is true if country C is a country with tag T
find_countries(T,C) :-
	info(C,_,_,_,L),
	contains(T,L).

% check_tags(L,C) is true if countries C has overlapping tags T with the countries in
% the list L
check_tags(L,C) :-
	info(C,_,_,_,T),
	tags_overlap(L,T).

% tags_overlap(L,T) is true if the list of tags T has
% at least one tag that is the same as a tag for one of the countries in L
tags_overlap([C|R],T) :-
	tags_overlap(R,T).
tags_overlap([C|R],T) :-
	info(C,_,_,_,Q),
	list_overlap(Q,T).

% list_overlap(L,Q) is true if L and Q have at least one element that is the same
list_overlap(L,[H|R]) :-
	contains(H,L).
list_overlap(L,[H|R]) :-
	list_overlap(L,R).

% contains(V,L) is true if list L contains the element V
contains(V,[V|T]).
contains(V,[H|T]) :-
	contains(V,T),
	dif(V,H).

% check_continent(N,C) is true if country C is in continent N
check_continent(N,C) :-
	info(C,N,_,_,_).

% check_weather(W, C) is true if the weather W is the weather of country C

% countries with weather 1: tropical
check_weather(1,C) :-
  info(C,_,1,_,_).

% countries with weather 2: Cold, winter like weather
check_weather(2,C) :-
info(C,_,2,_,_).

% countries with weather 3: mild, cloudy or sunny
check_weather(3,C) :-
  info(C,_,3,_,_).

% check_budget(B, C) is true if the country C is of a budget that satisfies the rule R
% Note: each of the rules are stated in the following lines

% countries with budget less than or equal to $
check_budget('=1', C) :-
  info(C,_,_,1,_).

% countries with budget greater than or equal to $
check_budget('>=1',C) :-
	info(C,_,_,1,_).

check_budget('>=1',C) :-
	info(C,_,_,2,_).

check_budget('>=1',C) :-
	info(C,_,_,3,_).

check_budget('>=1',C) :-
	info(C,_,_,4,_).

% countries with budget $$
check_budget('=2',C) :-
	info(C,_,_,2,_).

% countries with budget less than or equal to $$
check_budget('<=2',C) :-
	info(C,_,_,1,_).

check_budget('<=2',C) :-
	info(C,_,_,2,_).

% countries with budget greater than or equal to $$
check_budget('>=2',C) :-
	info(C,_,_,2,_).

check_budget('>=2',C) :-
	info(C,_,_,3,_).

check_budget('>=2',C) :-
	info(C,_,_,4,_).

% countries with budget $$$
check_budget('=3',C) :-
	info(C,_,_,3,_).

% countries with budget less than or equal to $$$
check_budget('<=3',C) :-
	info(C,_,_,1,_).

check_budget('<=3',C) :-
	info(C,_,_,2,_).

check_budget('<=3',C) :-
	info(C,_,_,3,_).

% countries with budget greater than or equal to $$$
check_budget('>=3',C) :-
	info(C,_,_,3,_).

check_budget('>=3',C) :-
	info(C,_,_,4,_).

% countries with budget $$$$
check_budget('=4',C) :-
	info(C,_,_,4,_).

% countries with budget less than or equal to $$$$
check_budget('<=4',C) :-
	info(C,_,_,1,_).

check_budget('<=4',C) :-
	info(C,_,_,2,_).

check_budget('<=4',C) :-
	info(C,_,_,3,_).

check_budget('<=4',C) :-
	info(C,_,_,4,_).

% countries with budget greater than or equal to $$$$
check_budget('>=4',C) :-
	info(C,_,_,4,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% All the following lines of code contain the contries and their descriptions
% that will be used for the queries

% info(C, N, W, B, T) is true if country C is in continent N, a weather W,
%   a budget B, and has tags T
% Note: tags are words that describe the class

% Asian Countries
info(china, 'Asia', 3, 2, ['food', 'culture', 'spicy', 'dimsum']).
info(sKorea, 'Asia', 3, 1, ['beauty', 'food', 'fashion']).
info(malaysia, 'Asia', 1, 1, ['food', 'culture', 'tropical', 'Southeast Asia']).
info(singapore, 'Asia', 1, 2, ['food', 'culture', 'tropical', 'Southeast Asia']).
info(indonesia, 'Asia', 1, 1, ['food', 'culture', 'tropical', 'Southeast Asia', 'beach']).
info(philippines, 'Asia', 1, 1,  ['food', 'culture', 'tropical', 'Southeast Asia', 'beach']).
info(brunei, 'Asia', 1, 1,  ['culture', 'outdoors', 'tropical', 'Southeast Asia']).
info(japan, 'Asia', 3, 2, ['food', 'culture', 'sushi', 'fashion', 'shopping']).
info(russia, 'Asia', 2, 3, ['cold', 'culture']).
info(kazakhstan, 'Asia', 2, 3,  ['cold', 'culture']).
info(india, 'Asia', 1, 1, ['food', 'hot', 'spices']).
info(hongKong, 'Asia', 1, 2, ['shopping', 'finance', ]).
info(thailand, 'Asia', 1, 1, ['tropical', 'spice', 'food']).
info(taiwan, 'Asia', 1, 1, ['food', 'night market']).


% North American courses
info(usa, 'NAmerica', 3, 2, ['outdoors', 'city']).
info(canada, 'NAmerica', 2, 2, ['safe', 'friendly', 'cold', 'wildlife', 'natural']).
info(mexico, 'NAmerica', 1, 1, ['warm', 'beaches']).


% South American Countries
info(brazil, 'SAmerica', 1, 2, ['tropical', 'beaches', 'food']).
info(argentina, 'SAmerica', 1, 2, ['hikes', 'outdoors', 'nature', 'food']).
info(peru, 'SAmerica', 1, 3, ['mountains', 'outdoors', 'hikes', 'food']).
info(colombia, 'SAmerica', 1, 2, ['coffee', 'farms', 'culture']).
info(chile, 'SAmerica', 1, 2, ['beach', 'outdoors', 'culture']).
info(venezuela, 'SAmerica', 1, 1, ['beaches', 'tropical']).
info(ecuador, 'SAmerica', 1, 1, ['beaches', 'tropical', 'marinelife']).

% African Countries
info(southafrica, 'Africa', 3, 4, ['wildlife', 'animals', 'nature', 'beaches']).
info(zimbabwe, 'Africa', 3, 3, ['wildlife', 'explore', 'nature', 'animals']).
info(kenya, 'Africa', 1, 3, ['wildlife', 'nature']).
info(nigeria, 'Africa', 1, 2, ['culture', 'beaches']).


% European Countries
info(france, 'Europe', 3, 3, ['romantic', 'food', 'culinary']).
info(uk, 'Europe', 3, 3, ['academics']).
info(germany, 'Europe', 3, 3, ['beer']).
info(spain, 'Europe', 1, 2, ['food', 'culture']).
info(switzerland, 'Europe', 2, 4, ['horology', 'expensive', 'chocolate', 'watches']).
info(netherlands, 'Europe', 3, 2, ['beer', 'alcohol']).
info(greece, 'Europe', 1, 2, ['beaches']).
