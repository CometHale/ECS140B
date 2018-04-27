/*  need  a user interface */
/* 		- via asserts: */
/*			- allow user to add db information */

/* starts the assistant */
clue_assistant :- game_setup, start_assistant.

/* gathers all game setup info from the user. */
game_setup :- set_players, set_rooms, set_weapons.

set_players :- 
	write('Enter player names in the order of play. Enter stop when all players have been entered:'),
	read(Player),
	dif(Player,stop),
	get_players().

get_players([]).
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


/* Hansen: Solution is a logical proof --- (For All P)(player(P), suspect(S), cantHave(P,S)) -> didIt(S)*/