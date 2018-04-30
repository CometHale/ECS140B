% Sally Ly, 999882177
% Haley Sanders-Turner, 912296300

/* DOCUMENTATION
Welcome to Clue Assistant. This program basically works as a notepad as you traverse through the mysteries of the Clue board. 
It can record all your moves and suggestions and will hold all the necessary information you need as well as tell you when to make an accusation.
However, it cannot tell you what your next suggestion can be nor can it track and other player's suggestions (and the cards shown to another player).

Before using our Clue Assistant, you must first set up the logistics:
create_players() : - takes in a list of players 
								 - Ex. create_players(["Miss Scarlet", "Mr. Green"]).
create_suspects() : - takes in a list of suspects
									- Ex. create_suspects(["Miss Scarlet", "Mr. Green", "Prof Plum"])
create_rooms() : takes in a list of rooms
create_weapons() : takes in a list of weapons
set_player_play_order : - what is the turn order of the players
												- takes in a Player and an Order Number 
												- Ex. set_player_play_order("Miss Scarlet", 1). (Miss Scarlet is the first person to move)

Once you have all the logistics set up and done, you can now use our 'print_db' command to view your notepad.
By simply typing 'print_db.', a list will be printed out with all the information you need to make the next suggestion or accusation.

When you first get your cards, you can put them into the database using the command:
mark_known_information(Card) - which will automatically mark the Card as a non-solution

You can record your suggestions by using the command:
made_suggestion(Suspect, Weapon, Room)

After making a suggestion, you can then record what the other players show you with the command:
received_information(Player, Card) where Player is the player who is showing you the card and Card is the card they are showing you. Also, 
the Clue Assistant will also automatically mark the cards you are shown as a non-solution.

You can also use the command 'enough_information' to check if you have enough evidence to make an accusation.
By simply typing 'enough_information.', it will either print 'false' (not enough evidence) or announce that you are able to make an accusation.

We hope you enjoy using our Clue Assistant! Happy murder solving!
*/

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
enough_information :- unchecked_weapon(W), unchecked_suspect(S), unchecked_room(R), writef("You have enough evidence! It's time to make an accusation.\nAccuse %w of murder using 	a %w in the %w!", [S, W, R]).
unchecked_weapon(L) :- findall(X, (weapon(X),not(not_solution_weapon(X))), Z), length(Z,N), N == 1,  nth0(0, Z, L).
unchecked_suspect(L) :- findall(X, (suspect(X),not(not_solution_suspect(X))), Z), length(Z,N), N == 1,  nth0(0, Z, L).
unchecked_room(L) :- findall(X, (room(X),not(not_solution_room(X))), Z), length(Z,N), N == 1,  nth0(0, Z, L).

/* Prints the contents of the database */
print_db:- 
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
	forall(suggestion(S, W, L), writef("%w, %w, %w\n", [S, W, L])).