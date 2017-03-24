
% find_identity(-Identity)
find_identity(Identity) :-
	(   part(4)
	->  my_agent(Agent)
	;   Agent = oscar),
	find_identity(Agent, Identity).

% find_identity(+Agent, -Identity)
find_identity(Agent, Identity) :-
	reset_discoveries(Agent),
	actors(Actors),
	ask_next_oracle(Agent, Actors, Identity),
	!.

ask_next_oracle(Agent, [], _Our_Identity) :-
	!,
	format("No Identities Remaining for ~w!\n", [Agent]),
	fail.

ask_next_oracle(_Agent, [Our_Identity], Our_Identity) :-
	!.

ask_next_oracle(Agent, Remaining_Actors, Our_Identity) :-
	length(Remaining_Actors, Remaining_Actors_Count),
	format("~w actors remaining\n", [Remaining_Actors_Count]),
	(   part(4)
	->  query_world(agent_current_position, [Agent, Start_Position]),
	    query_world(agent_current_energy, [Agent, Current_Energy])
	;   agent_current_position(Agent, Start_Position),
	    agent_current_energy(Agent, Current_Energy)),
	(   find_oracle(Agent, Start_Position, Current_Energy, o(O_N), Oracle_Position, Oracle_Path, Oracle_Cost)
	->  Energy_After_Oracle is Current_Energy - Oracle_Cost - 10,
	    (	find_station(Agent, Oracle_Position, Energy_After_Oracle, _Oracle_Charge_Station, _Oracle_Charge_Position, _Oracle_Charge_Path, _Oracle_Charge_Cost)
	    ->  go_to_oracle_and_ask(Agent, o(O_N), Oracle_Path, Remaining_Actors, Filtered_Actors),
		!,
		ask_next_oracle(Agent, Filtered_Actors, Our_Identity)
	    ;   go_refuel(Agent, Start_Position),
		!,
		ask_next_oracle(Agent, Remaining_Actors, Our_Identity))
	;   map_adjacent(Start_Position, _, c(_))
	->  !,
	    fail
	;   go_refuel(Agent, Start_Position),
	    !,
	    ask_next_oracle(Agent, Remaining_Actors, Our_Identity)).

ask_next_oracle(Agent, Remaining_Actors, _) :-
	format("\n~w cannot find oracle. Possible Identities:\n\n", [Agent]),
	!,
	member(Actor, Remaining_Actors),
	format("\t~w\n", [Actor]),
	fail.


go_refuel(Agent, Start_Position) :-
	find_station(Agent, Start_Position, 1000, Station, _Position, Path, _Cost),
	(   part(4)
	->  query_world(agent_do_moves, [Agent, Path]),
	    query_world(agent_topup_energy, [Agent, Station])
	;   agent_do_moves(Agent, Path),
	    agent_topup_energy(Agent, Station)).


find_oracle(Agent, Start_Position, Maximum_Cost, Oracle, Position, Path, Cost) :-
	(   find_discovered_oracle(Agent, Start_Position, Maximum_Cost, Oracle, Position, Path, Cost)
	;   find_new_oracle(Agent, Start_Position, Maximum_Cost, Oracle, Position, Path, Cost)).


find_discovered_oracle(Agent, Start_Position, Maximum_Cost, Oracle, Position, Path, Cost) :-
	best_discovered_oracle(Agent, Start_Position, Oracle, Position),
	find_path(go(Oracle, Position), Start_Position, Maximum_Cost, _End_Position, Path, Cost, _Depth, Discoveries),
	register_discoveries(Agent, Discoveries).


find_new_oracle(Agent, Start_Position, Maximum_Cost, Oracle, Position, Path, Cost) :-
	find_path(find(o(O_N)), Start_Position, Maximum_Cost, Position, Path, Cost, _Depth, Discoveries),
	register_discoveries(Agent, Discoveries),
	Oracle = o(O_N),
	(   part(4)
	->  \+ query_world(agent_check_oracle, [Agent, Oracle])
	;   \+ agent_check_oracle(Agent, Oracle)).


best_discovered_oracle(Agent, Position, o(O_N), Oracle_Position) :-
	all_discovered_oracles_acc(Agent, Position, [], Oracles),
	sort(3, @=<, Oracles, Sorted_Oracles),
	!,
	member(o(O_N, Oracle_Position, _), Sorted_Oracles).


all_discovered_oracles_acc(Agent, Position, Oracles, All_Oracles) :-
	discovered_oracle(Agent, Oracle, Oracle_Position),
	Oracle = o(N),
	\+ memberchk(o(N, Oracle_Position, _), Oracles),
	!,
	Cost = 0,
	calculate_heuristic(go(Oracle, Oracle_Position), Position, Cost, Heuristic),
	all_discovered_oracles_acc(Agent, Position, [o(N, Oracle_Position, Heuristic)|Oracles], All_Oracles).

all_discovered_oracles_acc(_Agent, _Position, Stations, Stations).


find_station(Agent, Start_Position, Maximum_Cost, Station, Position, Path, Cost) :-
	(   find_discovered_station(Agent, Start_Position, Maximum_Cost, Station, Position, Path, Cost)
	;   find_new_station(Agent, Start_Position, Maximum_Cost, Station, Position, Path, Cost)).


find_discovered_station(Agent, Start_Position, Maximum_Cost, Station, Position, Path, Cost) :-
	best_discovered_station(Agent, Start_Position, Station, Position),
	find_path(go(Station, Position), Start_Position, Maximum_Cost, _End_Position, Path, Cost, _Depth, Discoveries),
	register_discoveries(Agent, Discoveries).


find_new_station(Agent, Start_Position, Maximum_Cost, Station, Position, Path, Cost) :-
	find_path(find(c(N)), Start_Position, Maximum_Cost, Position, Path, Cost, _Depth, Discoveries),
	Station = c(N),
	register_discoveries(Agent, Discoveries).


best_discovered_station(Agent, Position, c(S_N), Station_Position) :-
	all_discovered_stations_acc(Agent, Position, [], Stations),
	sort(3, @=<, Stations, Sorted_Stations),
	!,
	member(c(S_N, Station_Position, _), Sorted_Stations).


all_discovered_stations_acc(Agent, Position, Stations, All_Stations) :-
	discovered_station(Agent, Station, Station_Position),
	Station = c(N),
	\+ memberchk(c(N, Station_Position, _), Stations),
	!,
	Cost = 0,
	calculate_heuristic(go(Station, Station_Position), Position, Cost, Heuristic),
	all_discovered_stations_acc(Agent, Position, [c(N, Station_Position, Heuristic)|Stations], All_Stations).

all_discovered_stations_acc(_Agent, _Position, Stations, Stations).


go_to_oracle_and_ask(Agent, Oracle, Oracle_Path, Remaining_Actors, Filtered_Actors) :-
	refuelling_move(Agent, Oracle_Path),
	!,
	(   part(4)
	->  query_world(agent_ask_oracle, [Agent, Oracle, link, Link])
	;   agent_ask_oracle(Agent, Oracle, link, Link)),
	retractall(discovered_oracle(Agent, Oracle, _)),
	filter_actors(Link, Remaining_Actors, Filtered_Actors).


refuelling_move(_Agent, []).

refuelling_move(Agent, [Position|Path]) :-
	(   part(4)
	->  query_world(agent_do_moves, [oscar, [Position]])
	;   agent_do_moves(Agent, [Position])),
	(   map_adjacent(Position, _, c(C_N))
	->  agent_topup_energy(Agent, c(C_N))
	;   true),
	refuelling_move(Agent, Path).


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


:- dynamic(discovered_oracle/3).
:- dynamic(discovered_station/3).

reset_discoveries(Agent) :-
	retractall(discovered_oracle(Agent, _, _)),
	retractall(discovered_station(Agent, _, _)).

register_discoveries(Agent, Discoveries) :-
	discovery_info(Discoveries, _Empty, Oracles, Stations),
	register_oracles(Agent, Oracles),
	register_stations(Agent, Stations).

register_oracles(_Agent, []).

register_oracles(Agent, [o(N, P)|Oracles]) :-
	(   (discovered_oracle(Agent, o(N), P) ; agent_check_oracle(Agent, o(N)))
	->  true
	;   format("~w found Oracle ~w at ~w\n", [Agent, N, P]),
	    assertz(discovered_oracle(Agent, o(N), P))),
	register_oracles(Agent, Oracles).



register_stations(_Agent, []).

register_stations(Agent, [c(N, P)|Stations]) :-
	(   discovered_station(Agent, c(N), P)
	->  true
	;   format("~w found Station ~w at ~w\n", [Agent, N, P]),
	    assertz(discovered_station(Agent, c(N), P))),
	register_stations(Agent, Stations).
