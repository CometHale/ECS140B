% Sally Ly, 999882177
% Haley Sanders-Turner, 912296300

/* DOCUMENTATION
Welcome to Clue Assistant. This program basically works as a notepad as you traverse through the mysteries of the Clue board. 
It can record all your moves and suggestions and will hold all the necessary information you need as well as tell you when to make an accusation.
However, it cannot tell you what your next suggestion can be nor can it track and other player's suggestions (and the cards shown to another player).

Simply type in "clue_assistant" to start our program and the program will have instructions for you to follow. 

When entering in the cards, players, weapons, rooms, please wrap them in quotes in order for it to be properly be entered into the database. Thank you. 

When entering in the Turn Order, please enter in a number. For example, if Miss Scarlet is going first, then her Turn Order would be 1 and if Professor Plum
goes second, then her Turn Order would be 2. 

We hope you enjoy using our Clue Assistant! Happy murder solving!
*/

%Documentation
help :- 
	writeln("Welcome to Clue Assistant. This program basically works as a notepad as you traverse through the mysteries of the Clue board.\nIt can record all your moves and suggestions and will hold all the necessary information you need as well as tell you when to make an accusation.\nHowever, it cannot tell you what your next suggestion can be nor can it track and other player\'s suggestions (and the cards shown to another player).\n\nWhen entering in the cards, players, weapons, rooms, please wrap them in quotes in order for it to be properly be entered into the database. \n\nWhen entering in the Turn Order, please enter in a number. For example, if Miss Scarlet is going first, then her Turn Order would be 1 and if Professor Plum goes second, then her Turn Order would be 2. \n\nWe hope you enjoy using our Clue Assistant! Happy murder solving!\n").

/* DATABASE PREDICATES */
/* player(Name) */
/* weapon(Name) */
/* suspect(Name) */
/* room(Name) */
/* play_order(Player, OrderNumber) */
/* not_solution_weapon(Name) */
/* not_solution_room(Name) */
/* not_solution_suspect(Name) */
/* player_information(Player,Information) */
/* suggestion(Suspect,Weapon,Room) */

/* default facts so no error shows up when printing an empty db */
player("empty").
suspect("empty").
room("empty").
weapon("empty").
play_order("empty", 0).
not_solution_weapon("empty").
not_solution_room("empty").
not_solution_suspect("empty").
player_information("empty", "empty").
suggestion("empty", "empty", "empty").

/* Takes in a list of players and recursively creates the individual player predicates */
create_players([]) :- retract(player("empty")).
create_players([H|T]) :- assertz(player(H)), create_players(T).

/* Takes in a list of suspects and recursively creates the individual suspect predicates */
create_suspects([])  :- retract(suspect("empty")).
create_suspects([H|T]) :- assertz(suspect(H)), create_suspects(T).

/* Takes in a list of rooms and recursively creates the individual room predicates */
create_rooms([])  :- retract(room("empty")).
create_rooms([H|T]) :- assertz(room(H)), create_rooms(T).

/* Takes in a list of weapons and recursively creates the individual weapon predicates */
create_weapons([])  :- retract(weapon("empty")).
create_weapons([H|T]) :- assertz(weapon(H)), create_weapons(T).

/* Takes in [Player, OrderNumber] and creates a predicate marking the player's place in the order of play */
set_player_play_order(Player,Order) :- play_order("empty", 0), player(Player), assertz(play_order(Player,Order)), retract(play_order("empty", 0)).
set_player_play_order(Player,Order) :- player(Player), assertz(play_order(Player,Order)).

/* Takes in [CardValue] and creates a predicate marking the CardValue as not a part of the solution. */
mark_known_information(CardValue) :- not_solution_weapon("empty"), weapon(CardValue), assertz(not_solution_weapon(CardValue)), retract(not_solution_weapon("empty")).
mark_known_information(CardValue) :- weapon(CardValue), assertz(not_solution_weapon(CardValue)).

mark_known_information(CardValue) :- not_solution_room("empty"), room(CardValue), assertz(not_solution_room(CardValue)), retract(not_solution_room("empty")).
mark_known_information(CardValue) :- room(CardValue), assertz(not_solution_room(CardValue)).

mark_known_information(CardValue) :- not_solution_suspect("empty"), suspect(CardValue), assertz(not_solution_suspect(CardValue)), retract(not_solution_suspect("empty")).
mark_known_information(CardValue) :- suspect(CardValue), assertz(not_solution_suspect(CardValue)).

/* Takes in [Player, Information] and creates a predicate that marks that a particular player gave information */
/* Information: card value of card shown to user by Player, or OtherPlayer that Player showed a card to */
received_information(Player, Info) :- player_information("empty", "empty"), weapon(Info), mark_known_information(Info), assertz(player_information(Player, Info)), retract(player_information("empty", "empty")).
received_information(Player, Info) :- weapon(Info), mark_known_information(Info), assertz(player_information(Player, Info)).

received_information(Player, Info) :- player_information("empty", "empty"), room(Info), mark_known_information(Info), assertz(player_information(Player, Info)), retract(player_information("empty", "empty")).
received_information(Player, Info) :- room(Info), mark_known_information(Info), assertz(player_information(Player, Info)).

received_information(Player, Info) :- player_information("empty", "empty"), suspect(Info), mark_known_information(Info), assertz(player_information(Player, Info)), retract(player_information("empty", "empty")).
received_information(Player, Info) :- suspect(Info), mark_known_information(Info), assertz(player_information(Player, Info)).

received_information(Player, Info) :- player_information("empty", "empty"), player(Info), assertz(player_information(Player, Info)), retract(player_information("empty", "empty")). /* may run into issues if a particular player is showing one other particular player a lot of information */
received_information(Player, Info) :- player(Info), assertz(player_information(Player, Info)).

/* Takes in a user's suggestion and creates a predicate for it. */
made_suggestion(Suspect,Weapon,Room) :- suggestion("empty", "empty", "empty"), suspect(Suspect), weapon(Weapon), room(Room), assertz(suggestion(Suspect,Weapon,Room)), retract(suggestion("empty", "empty", "empty")).
made_suggestion(Suspect,Weapon,Room) :- suspect(Suspect), weapon(Weapon), room(Room), assertz(suggestion(Suspect,Weapon,Room)).

/* Determines if there's enough information to make an accusation */
/* There's enough information to make an accusation if there are only one of each card type left unaccounted for */
enough_information :- unchecked_weapon(W), unchecked_suspect(S), unchecked_room(R), writef("You have enough evidence! It's time to make an accusation.\nAccuse %w of murdering someone in the %w by using a %w!\n\n", [S, R, W]).
unchecked_weapon(L) :- findall(X, (weapon(X),not(not_solution_weapon(X))), Z), length(Z,N), N == 1,  nth0(0, Z, L).
unchecked_suspect(L) :- findall(X, (suspect(X),not(not_solution_suspect(X))), Z), length(Z,N), N == 1,  nth0(0, Z, L).
unchecked_room(L) :- findall(X, (room(X),not(not_solution_room(X))), Z), length(Z,N), N == 1,  nth0(0, Z, L).

/* Prints the contents of the database */
print_db:- 
	nl,
	write("Player(s):"), 
	nl,
	forall(player(P), writeln(P)),
	nl,
	write("Order of Play:"),
	nl,
	forall(play_order(Plyr,O), writef("%w, %w\n", [Plyr, O])),
	nl,
	write("Weapon(s):"),
	nl,
	forall(weapon(W), writeln(W)),
	nl,
	write("Suspect(s):"),
	nl,
	forall(suspect(S),writeln(S)),
	nl,
	write("Room(s):"),
	nl,
	forall(room(R),writeln(R)),
	nl,
	write("Non-Solution Weapon(s):"),
	nl,
	forall(not_solution_weapon(Wpn), writeln(Wpn)),
	nl,
	write("Non-Solution Suspect(s):"),
	nl,
	forall(not_solution_suspect(Sspct), writeln(Sspct)),
	nl,
	write("Non-Solution Room(s):"),
	nl,
	forall(not_solution_room(Rm), writeln(Rm)),
	nl,
	write("Information from Player(s):"),
	nl,
	forall(player_information(Pl,Info), writef("%w, %w\n", [Pl, Info])),
	nl,
	write("Previous Suggestion(s):"),
	nl,
	forall(suggestion(S, W, L), writef("%w, %w, %w\n", [S, W, L])),
	nl.

/* Get Functions */
getPlayers([Player|List]) :- 
    writeln("Enter Player Name (Enter 'stop' to finish): "), 
    read(Player),
    dif(Player, stop),
    getPlayers(List).

getPlayers([]).

getRooms([Room|List]) :- 
    writeln("Enter Room Name (Enter 'stop' to finish): "), 
    read(Room),
    dif(Room, stop),
    getRooms(List).

getRooms([]).

getSuspects([Suspect|List]) :- 
    writeln("Enter Suspect Name (Enter 'stop' to finish): "), 
    read(Suspect),
    dif(Suspect, stop),
    getSuspects(List).

getSuspects([]).

getWeapons([Weapon|List]) :- 
    writeln("Enter Weapon Name (Enter 'stop' to finish): "), 
    read(Weapon),
    dif(Weapon, stop),
    getWeapons(List).

getWeapons([]).

getPlayOrder([Player|List]) :-
 	writeln("Enter the Player Name: (Enter 'stop' to finish)"),
 	read(Player),
 	dif(Player, stop),
 	writeln("Enter the Player's Turn Order: "),
 	read(Order),
 	set_player_play_order(Player, Order),
 	getPlayOrder(List).

getPlayOrder([]).

getCards([Card|List]) :-
		writeln("Enter Your Card (Enter 'stop' to finish): "), 
    read(Card),
    dif(Card, stop),
    mark_known_information(Card),
    getCards(List).

getCards([]).

getSuggestion() :-
	writeln("Enter your Suggested Suspect: "),
	read(Suspect),
	writeln("Enter your Suggested Weapon: "),
	read(Weapon),
	writeln("Enter your Suggested Room: "),
	read(Room),
	made_suggestion(Suspect, Weapon, Room),
	writeln("Suggestion Added\n").

getInformation() :-
	writeln("Enter the Player who has shown you a card: "),
	read(Player),
	writeln("Enter the Card they showed you: "),
	read(Card),
	received_information(Player, Card),
	writeln("New Information Added To Database\n").

useInput(Num) :- Num is 1, setup_game.
useInput(Num) :- Num is 2, print_db.
useInput(Num) :- Num is 3, getCards(X).
useInput(Num) :- Num is 4, getSuggestion.
useInput(Num) :- Num is 5, getInformation.
useInput(Num) :- Num is 6, enough_information.
useInput(Num) :- Num is 6, not(enough_information), writeln("You don't have enough information to make an accusation yet. Keep investigating!\n").
useInput(Num) :- Num is 7, help.

getUserInput([Num|List]) :-
	writeln("What would you like to do? Enter the corresponding number. (Enter 'stop' to finish)\n 1) Set Up the Game Board\n 2) Print Database\n 3) Enter your cards into the database\n 4) Record your Suggestion\n 5) Record what other players show you\n 6) Check if you can make an accusation\n 7) Documentation"),
	read(Num),
	dif(Num, stop),
	useInput(Num),
	getUserInput(List).

getUserInput([]).

/* Clue Assistant Interface */
clue_assistant :-
	getUserInput(List),
	writeln("Clue Assistant is shutting down. Thank you for using us.").

setup_game :- 
	writeln("Enter Players"),
	getPlayers(Players),
	create_players(Players), 
	writeln("Players Added\n"),
	writeln("Enter Player Turn Order"),
	getPlayOrder(Order),
	writeln("Player Turn Order Added\n"),
	writeln("Enter Suspects"),
	getSuspects(Suspects),
	create_suspects(Suspects),
	writeln("Suspects Added\n"),
	writeln("Enter Weapons"),
	getWeapons(Weapons),
	create_weapons(Weapons),
	writeln("Weapons Added\n"),
	writeln("Enter Rooms"),
	getRooms(Rooms),
	create_rooms(Rooms),
	writeln("Rooms Added\n").