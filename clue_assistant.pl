/* need  a user interface */
/* need a way to track whose turn it currently is */
/* need a way to make sure duplicates of rooms or players or weapons don't happen */
/* need a way to print the database */
/* might be better start db with predicates for all possible solution weapons, suspects and rooms, then remove predicates info is discovered */


/* Takes in a list of players and recursively creates the individual player predicates */
create_players([]).
create_players([H|T]) :- assertz(player(H)), create_players(T).

/* Takes in a list of suspects and recursively creates the individual suspect predicates */
create_suspects([]).
create_suspects([H|T]) :- assertz(suspect(H)), create_suspects(T).

/* Takes in a list of rooms and recursively creates the individual room predicates */
create_rooms([]).
create_rooms([H|T]) :- assertz(room(H)), create_rooms(T).

/* Takes in a list of weapons and recursively creates the individual weapon predicates */
create_weapons([]).
create_weapons([H|T]) :- assertz(weapon(H)), create_weapons(T).

/* Takes in [Player, OrderNumber] and creates a predicate marking the player's place in the order of play */
set_player_play_order(Player,Order) :- assertz(play_order(Player,Order)).

/* Takes in [CardValue] and creates a predicate marking the CardValue as not a part of the solution. */
mark_known_information(CardValue) :- weapon(CardValue), assertz(not_solution_weapon(CardValue)).
mark_known_information(CardValue) :- room(CardValue), assertz(not_solution_room(CardValue)).
mark_known_information(CardValue) :- suspect(CardValue), assertz(not_solution_suspect(CardValue)).

/* Takes in [Player, Information] and creates a predicate that marks that a particular player gave information */
/* Information: card value of card shown to user by Player, or OtherPlayer that Player showed a card to */
received_information(Player, Info) :- weapon(Info), mark_known_information(Info), assertz(player_information(Player, Info)).
received_information(Player, Info) :- room(Info), mark_known_information(Info), assertz(player_information(Player, Info)).
received_information(Player, Info) :- suspect(Info), mark_known_information(Info), assertz(player_information(Player, Info)).
received_information(Player, Info) :- player(Info), assertz(player_information(Player, Info)). /* may run into issues if a particular player is showing one other particular player a lot of information */

/* Takes in a user's suggestion and creates a predicate for it. */
made_suggestion(Suspect,Weapon,Room) :- assertz(suggestion(Suspect,Weapon,Room)).

/* Determines if there's enough information to make an accusation */
/* There's enough information to make an accusation if there are only one of each card type left unaccounted for */
enough_information :- unchecked_weapon, unchecked_suspect, unchecked_room.
unchecked_weapon :- findall(weapon,not(not_solution_weapon(X)), Z), length(Z,N), N == 1.
unchecked_suspect :- findall(suspect,not(not_solution_suspect(X)), Z), length(Z,N), N == 1.
unchecked_room :- findall(room,not(not_solution_room(X)), Z), length(Z,N), N == 1.


/* OLD CODE THAT MAY BE HELPFUL LATER */
/* set_players :- write('List player names in the order of play, separated by commas:'), nl, get_players. */
/* get_players :- read_string(X), atom_string(X,Players_string), split_string(Players_string, ",", "", Players), write(Players).*/

/* 		- via asserts: */
/*			- allow user to add db information */

/* starts the assistant */
/*
# clue_assistant :- 
# 	game_setup,
# 	start.

# start :- done. # Checks if the end condition has been reached.
# start :- # update db and prepare for next turn/round of inputs
# 	start.


/* gathers all game setup info from the user. */
# game_setup :- set_players, set_rooms, set_weapons.

set_players :- write('List player names in the order of play, separated by commas:'),
	read_string(Players_string),
	split_string(Players_string, ",", "", Players).
	# ,
	# create_players(Players).

# create_players([]).
# create_players([H|T]) :- assert(player(H)), create_players(T).

# create_player :- stop.	
# create_player :-
# 	write('Player:'),

	# read(Player),
	# dif(Player,stop).
/* prints out the database */
# print_db([]).
# print_db([H|T]) :- write(H), nl, print_db(T).

/* determines if enough information is known to make an accusation. */
/* enough information is known when: */
/* 			- there is only 1 Location, 1 Weapon and 1 Person left unaccounted for.  */
/* 				- 1 Location that no players can have AND 1 Weapon no players can have AND 1 Person no players can have */
# enough_information() :- .

/* determines whether or not a player could have a specific card */
/* a player can't have a particular card if: */
/* 		- the card is in another player's hand OR the card is apart of the solution */
/*		- */
# cantHave(Player, Card) :- . 

#  utils

# different(X,Y) :- .
/* Hansen: Solution is a logical proof --- (For All P)(player(P), suspect(S), cantHave(P,S)) -> didIt(S)*/

*/