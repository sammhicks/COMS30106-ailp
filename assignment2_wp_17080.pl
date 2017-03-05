% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(Identity) :-
	reset_discoveries,
	actors(Actors),
	ask_next_oracle(Actors, Identity),
	!.

ask_next_oracle([], _Our_Identity) :-
	!,
	format("No Identities Remaining!\n"),
	fail.

ask_next_oracle([Our_Identity], Our_Identity) :-
	!.

ask_next_oracle(Remaining_Actors, Our_Identity) :-
	agent_current_position(oscar, Start_Position),
	agent_current_energy(oscar, Current_Energy),
	(   find_oracle(Start_Position, Current_Energy, o(O_N), Oracle_Position, Oracle_Path, Oracle_Cost)
	->  Energy_After_Oracle is Current_Energy - Oracle_Cost - 10,
	    (	find_station(Oracle_Position, Energy_After_Oracle, _Oracle_Charge_Station, _Oracle_Charge_Position, _Oracle_Charge_Path, _Oracle_Charge_Cost)
	    ->  go_to_oracle_and_ask(o(O_N), Oracle_Path, Remaining_Actors, Filtered_Actors),
		!,
		ask_next_oracle(Filtered_Actors, Our_Identity)
	    ;   go_refuel(Start_Position, Current_Energy),
		ask_next_oracle(Remaining_Actors, Our_Identity))
	;   map_adjacent(Start_Position, _, c(_)) % If we're next to a station and can't find an oracle, it means that we can't find an oracle at all
	->  !,
	    fail
	;   go_refuel(Start_Position, Current_Energy),
	    ask_next_oracle(Remaining_Actors, Our_Identity)).

ask_next_oracle(Remaining_Actors, _) :-
	!,
	format("\n\nCannot find oracle. Possible Identities: ~w\n\n\n", [Remaining_Actors]),
	fail.


go_refuel(Start_Position, Current_Energy) :-
	find_station(Start_Position, Current_Energy, Station, _Position, Path, _Cost),
	agent_do_moves(oscar, Path),
	agent_topup_energy(oscar, Station).


find_oracle(Start_Position, Maximum_Cost, Oracle, Oracle_Position, Oracle_Path, Cost) :-
	(   find_discovered_oracle(Start_Position, Maximum_Cost, Oracle, Oracle_Position, Oracle_Path, Cost)
	;   find_path(find(o(O_N)), Start_Position, Maximum_Cost, Oracle_Position, Oracle_Path, Cost, _Depth, Discoveries),
	    register_discoveries(Discoveries),
	    Oracle = o(O_N),
	    \+ agent_check_oracle(oscar, Oracle)).


find_discovered_oracle(Start_Position, Maximum_Cost, Oracle, Position, Path, Cost) :-
	best_discovered_oracle(Start_Position, Oracle, Position),
	find_path(go(Oracle, Position), Start_Position, Maximum_Cost, _End_Position, Path, Cost, _Depth, Discoveries),
	register_discoveries(Discoveries).


best_discovered_oracle(Position, o(O_N), Oracle_Position) :-
	all_discovered_oracles_acc(Position, [], Oracles),
	sort(3, @=<, Oracles, Sorted_Oracles),
	!,
	member(o(O_N, Oracle_Position, _), Sorted_Oracles).

all_discovered_oracles_acc(Position, Oracles, All_Oracles) :-
	discovered_oracle(Oracle, Oracle_Position),
	Oracle = o(N),
	\+ memberchk(o(N, Oracle_Position, _), Oracles),
	!,
	calculate_heuristic(go(Oracle, Oracle_Position), Position, Heuristic),
	all_discovered_oracles_acc(Position, [o(N, Oracle_Position, Heuristic)|Oracles], All_Oracles).

all_discovered_oracles_acc(_Position, Stations, Stations).


find_station(Start_Position, Maximum_Cost, Station, Position, Path, Cost) :-
	(   find_discovered_station(Start_Position, Maximum_Cost, Station, Position, Path, Cost)
	;   find_path(find(c(C_N)), Start_Position, Maximum_Cost, Position, Path, Cost, _Depth, Discoveries),
	    Station = c(C_N),
	    register_discoveries(Discoveries)).


find_discovered_station(Start_Position, Maximum_Cost, Station, Position, Path, Cost) :-
	best_discovered_station(Start_Position, Station, Position),
	find_path(go(Station, Position), Start_Position, Maximum_Cost, _End_Position, Path, Cost, _Depth, Discoveries),
	register_discoveries(Discoveries).


best_discovered_station(Position, c(S_N), Station_Position) :-
	all_discovered_stations_acc(Position, [], Stations),
	sort(3, @=<, Stations, Sorted_Stations),
	!,
	member(c(S_N, Station_Position, _), Sorted_Stations).


all_discovered_stations_acc(Position, Stations, All_Stations) :-
	discovered_station(Station, Station_Position),
	Station = c(N),
	\+ memberchk(c(N, Station_Position, _), Stations),
	!,
	calculate_heuristic(go(Station, Station_Position), Position, Heuristic),
	all_discovered_stations_acc(Position, [c(N, Station_Position, Heuristic)|Stations], All_Stations).

all_discovered_stations_acc(_Position, Stations, Stations).


go_to_oracle_and_ask(Oracle, Oracle_Path, Remaining_Actors, Filtered_Actors) :-
	refuelling_move(Oracle_Path),
	!,
	agent_ask_oracle(oscar, Oracle, link, Link),
	retractall(discovered_oracle(Oracle, _)),
	filter_actors(Link, Remaining_Actors, Filtered_Actors).


refuelling_move([]).

refuelling_move([Position|Path]) :-
	agent_do_moves(oscar, [Position]),
	(   map_adjacent(Position, _, c(C_N))
	->  agent_topup_energy(oscar, c(C_N))
	;   true),
	refuelling_move(Path).


actors(Actors) :-
	actors([], Actors).

actors(Current_Actors, All_Actors) :-
	actor(Actor),
	\+ memberchk(Actor, Current_Actors),
	!,
	actors([Actor|Current_Actors], All_Actors).

actors(Actors, Actors).

filter_actors(_Link, [], []).

filter_actors(Link, [Actor|Actors], All_Filtered_Actors) :-
	(   wp(Actor, WikiText), wt_link(WikiText, Link)
	->  All_Filtered_Actors = [Actor|Filtered_Actors]
	;   All_Filtered_Actors = Filtered_Actors),
	!,
	filter_actors(Link, Actors, Filtered_Actors).


:- dynamic(discovered_oracle/2).
:- dynamic(discovered_station/2).

reset_discoveries :-
	retractall(discovered_oracle(_, _)),
	retractall(discovered_station(_, _)).

register_discoveries(Discoveries) :-
	discovery_info(Discoveries, _Empty, Oracles, Stations),
	register_oracles(Oracles),
	register_stations(Stations).

register_oracles([]).

register_oracles([o(N, P)|Oracles]) :-
	(   (discovered_oracle(o(N), P) ; agent_check_oracle(oscar, o(N)))
	->  true
	;   format("Found Oracle ~w at ~w\n", [N, P]),
	    assertz(discovered_oracle(o(N), P))),
	register_oracles(Oracles).



register_stations([]).

register_stations([c(N, P)|Stations]) :-
	(   discovered_station(c(N), P)
	->  true
	;   format("Found Station ~w at ~w\n", [N, P]),
	    assertz(discovered_station(c(N), P))),
	register_oracles(Stations).
