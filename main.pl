% Travel Destination Recommender
% CPSC 312 2019 Project #2
% Written by Sandy Co and Kyle Mas


% starting instructions for user
start :- write("Welcome to our Travel Destination Recommender."),nl,
         sleep(0.2),
		 write("To get started, type ?- get(DestinationRecs) and follow each step
		        to get a list of course recommendations.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% All the following lines of code contain the courses and their descriptions
% that will be used for the queries

% info(C, D, T) is true if country C is in continent D, S a list of similar countries has tags T
% Note: tags are words that describe the class

% Asian Countries
info(China, Asia, [HongKong, Taiwan], ['']).
info(SKorea, Asia, [Japan, China, Taiwan], ['']).
info(Malaysia, Asia, [Philippines, Brunei, Singapore, Indonesia], ['']).
info(Singapore, Asia, [Philippines, Brunei, Malaysia, Indonesia], ['']).
info(Indonesia, Asia, [Philippines, Brunei, Singapore, Malaysia], ['']).
info(Philippines, Asia, [Malaysia, Brunei, Singapore, Indonesia], ['']).
info(Brunei, Asia, [Philippines, Malaysia, Singapore, Indonesia], ['']).
info(Japan, Asia, [China, SKorea, Taiwan], ['']).
info(Russia, Asia, ['']).
info(India, Asia, ['']).
info(HongKong, Asia, [Taiwan, China], ['']).
info(Thailand, Asia, [Philippines, Brunei, Singapore, Indonesia], ['']).
info(Taiwan, Asia, [HongKong, China], ['']).


% North American courses
info(USA, NAmerica, ['']).
info(Canada, NAmerica, ['']).
info(Mexico, NAmerica, ['']).


% Soinfo(USA, NAmerica, ['']).
info(Brazil, SAmerica, ['']).
info(Argentina, SAmerica, ['']).
info(Peru, SAmerica, ['']).
info(Colombia, SAmerica, ['']).
info(Chile, SAmerica, ['']).
info(Venezuela, SAmerica, ['']).
info(Ecuador, SAmerica, ['']).


% European Countries
info(France, Europe, ['']).
info(UK, Europe, ['']).
info(Germany, Europe, ['']).
info(Spain, Europe, ['']).
info(Switzerland, Europe, ['']).
info(Netherlands, Europe, ['']).
info(Poland, Europe, ['']).
info(Greece, Europe, ['']).
