candidate_number(17080).

solve_task(Task,Cost) :-
	(   part(1) -> solve_task_1_3(Task, Cost)
	;   part(3) -> solve_task_1_3(Task, Cost)
	%  ;   part(4) -> solve_task_4(Task, Cost)
	).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_1_3(+Task, -Cost)
solve_task_1_3(Task, [cost(Cost), depth(Depth)]) :-
	agent_current_position(oscar, Start_Position),
	agent_current_energy(oscar, Start_Energy),
	find_path(Task, Start_Position, Start_Energy, _End_Position, Path, Cost, Depth, _Discoveries),
	!, % Once we've found a solution, we hope that it's the best one
	agent_do_moves(oscar, Path),
	!. % we're done, prune any remaing choice points

% find_path(+Task, +Start_Position, +Maximum_Cost, -End_Position, -Path,
% -Cost, -Depth, -State)
find_path(Task, Start_Position, Maximum_Cost, End_Position, Path, Final_Cost, Final_Depth, Discoveries) :-
	init_state(Task, Start_Position, Start_State),
	solve_task_bfs(
	    Task,
	    Maximum_Cost,
	    Start_State,
	    path(Final_Cost, Final_Depth, _Final_Heuristic, Reverse_Path),
	    Discoveries
	),
	Reverse_Path = [End_Position|_],
	reverse(Reverse_Path, [_Start_Position|Path]).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%solve_task_4(Task,Cost):-
%  my_agent(Agent),
%  query_world( agent_current_position, [Agent,P] ),
%  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),
%  reverse(R,[_Init|Path]),
%  query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_bfs(+Task, +Maximum_Cost, -Current_State, -Final_Path,
% -Discoveries)
solve_task_bfs(Task, _Maximum_Cost, State, Path, Discoveries) :-
	achieved(Task, State, Path, Discoveries).

solve_task_bfs(Task, Maximum_Cost, Current_State, Final_Path, Final_Discoveries) :-
	state_info(Current_State, Current_Agenda, Current_Visited_Positions, Current_Discoveries),
	Current_Agenda = [path(Cost, Depth, _Heuristic, Path)|Other_Path_Functors],
	Path = [Current_Position|_],
	find_adjacent_discoveries(Current_Position, Current_Discoveries, New_Discoveries),
	discovery_info(New_Discoveries, Empty_Positions, _Oracles, _Stations),
	clear_empty_discoveries(New_Discoveries, Discoveries_Without_Empty),
	prepend_to_paths(Path, Empty_Positions, New_Paths),
	construct_paths(Task, Cost, Depth, New_Paths, New_Path_Functors),
	filter_positions(Current_Visited_Positions, Maximum_Cost, New_Path_Functors, Filtered_New_Path_Functors),
	insert_previous_paths(Filtered_New_Path_Functors, Current_Visited_Positions, New_Visited_Positions),
	append(Other_Path_Functors, Filtered_New_Path_Functors, New_Agenda),
	(   heuristic_sortable(Task)
	->  sort(3, @=<, New_Agenda, Sorted_New_Agenda)
	;   Sorted_New_Agenda = New_Agenda),
	state_info(New_State, Sorted_New_Agenda, New_Visited_Positions, Discoveries_Without_Empty),
	!,
	solve_task_bfs(Task, Maximum_Cost, New_State, Final_Path, Final_Discoveries).


% bfs state
state_info(state(Agenda, Visited_Positions, Discoveries), Agenda, Visited_Positions, Discoveries).

% initial_state(-State)
init_state(Task, Start_Position, State) :-
	Cost = 0,
	Depth = 0,
	calculate_heuristic(Task, Start_Position, Start_Heuristic),
	Agenda = [path(Cost, Depth, Start_Heuristic, [Start_Position])],
	init_visited_positions(Init_Visited_Positions),
	insert_visited_position(Start_Position, Cost, Init_Visited_Positions, Visited_Positions),
	init_discoveries(Discoveries),
	state_info(State, Agenda, Visited_Positions, Discoveries).

% visited positions

init_visited_positions(positions{}).

% query_position(+Position, +State, -Cost)
already_visited_position(Visited_Positions, p(X, Y), Cost) :-
	get_dict(Y, Visited_Positions, Row),
	get_dict(X, Row, Cost).


insert_previous_paths([], Visited_Positions, Visited_Positions).

insert_previous_paths([path(Cost, _Depth, _Heuristic, [Position|_])|Paths], Current_Visited_Positions, Final_Visited_Positions) :-
	insert_visited_position(Position, Cost, Current_Visited_Positions, New_Visited_Positions),
	insert_previous_paths(Paths, New_Visited_Positions, Final_Visited_Positions).


insert_visited_position(p(X, Y), Cost, Current_Positions, New_Positions) :-
	(   get_dict(Y, Current_Positions, Current_Row)
	->  true
	;   init_visited_positions(Current_Row)
	),
	put_dict(X, Current_Row, Cost, New_Row),
	put_dict(Y, Current_Positions, New_Row, New_Positions).

% discoveries
discovery_info(discoveries(Empty, Oracles, Stations), Empty, Oracles, Stations).

init_discoveries(Discoveries) :-
	discovery_info(Discoveries, [], [], []).


clear_empty_discoveries(Current_Discoveries, New_Discoveries) :-
	discovery_info(Current_Discoveries, _Empty, Oracles, Stations),
	discovery_info(New_Discoveries, [], Oracles, Stations).


find_adjacent_discoveries(Current_Position, Current_Discoveries, Final_Discoveries) :-
	map_adjacent(Current_Position, Discovery_Position, Discovery),
	(   Discovery = empty
	->  discovery_info(Current_Discoveries, Empty, Oracles, Stations),
	    \+ memberchk(Discovery_Position, Empty),
	    discovery_info(New_Discoveries, [Discovery_Position|Empty], Oracles, Stations)
	;   Discovery = o(O_N)
	->  discovery_info(Current_Discoveries, Empty, Oracles, Stations),
	    \+ memberchk(o(O_N, _), Oracles),
	    discovery_info(New_Discoveries, Empty, [o(O_N, Discovery_Position)|Oracles], Stations)
	;   Discovery = c(C_N)
	->  discovery_info(Current_Discoveries, Empty, Oracles, Stations),
	    \+ memberchk(c(C_N, _), Stations),
	    discovery_info(New_Discoveries, Empty, Oracles, [c(C_N, Discovery_Position)|Stations])),
	!,
	find_adjacent_discoveries(Current_Position, New_Discoveries, Final_Discoveries).

find_adjacent_discoveries(_Current_Position, Discoveries, Discoveries).

% achieved(+Task, +Current_State, -Path, -Discoveries)
achieved(go(Position), State, Path, Discoveries) :-
	state_info(State, [Path|_], _Visited_Positions, Discoveries),
	Path = path(_Final_Cost, _Final_Depth, _Final_Heuristic, [Position|_]).

achieved(find(Target), State, Path, Final_Discoveries) :-
	state_info(State, [Path|_], _Visited_Positions, Discoveries),
	Path = path(_Final_Cost, _Final_Depth, _Final_Heuristic, [Final_Position|_]),
	map_adjacent(Final_Position, _, Target),
	find_adjacent_discoveries(Final_Position, Discoveries, Final_Discoveries).

achieved(go(Target, Target_Position), State, Path, Discoveries) :-
	state_info(State, [Path|_], _Visited_Positions, Discoveries),
	Path = path(_Final_Cost, _Final_Depth, _Final_Heuristic, [Position|_]),
	map_adjacent(Position, Target_Position, Target).


% filter_positions(+Visited_Positions, +Positions,
% -All_Filtered_Positions)
filter_positions(_Visited_Positions, _Maximum_Cost, [], []).

filter_positions(Visited_Positions, Maximum_Cost, [Path|Paths], All_Filtered_Paths) :-
	(   cost_too_high(Maximum_Cost, Path)
	;   better_path_found(Visited_Positions, Path)
	->  All_Filtered_Paths = Filtered_Paths
	;   All_Filtered_Paths = [Path|Filtered_Paths]
	),
	filter_positions(Visited_Positions, Maximum_Cost, Paths, Filtered_Paths).

cost_too_high(Maximum_Cost, path(Cost, _Depth, _Heuristic, _Path)) :-
	Cost > Maximum_Cost.

better_path_found(Visited_Positions, path(Cost, _Depth, _Heuristic, [Position|_])) :-
	already_visited_position(Visited_Positions, Position, Visited_Cost),
	Visited_Cost =< Cost.


% prepend_to_paths(+Previous_Path, +New_End_Positions, -New_Paths)
prepend_to_paths(_Previous_Path, [], []).

prepend_to_paths(Previous_Path, [End_Position|End_Positions], [[End_Position|Previous_Path]|New_Paths]) :-
  prepend_to_paths(Previous_Path, End_Positions, New_Paths).


construct_paths(_Task, _Current_Cost, _Current_Depth, [], []).

construct_paths(Task, Current_Cost, Current_Depth, [Path|Paths], [Heuristic|Heuristics]) :-
	construct_path(Task, Current_Cost, Current_Depth, Path, Heuristic),
	construct_paths(Task, Current_Cost, Current_Depth, Paths, Heuristics).


construct_path(Task, Current_Cost, Current_Depth, Path, path(New_Cost, New_Depth, Heuristic, Path)) :-
	New_Cost is Current_Cost + 1,
	New_Depth is Current_Depth + 1,
	Path = [Current_Position|_],
	calculate_heuristic(Task, Current_Position, Heuristic).

calculate_heuristic(go(Target), Current_Position, Heuristic) :-
	map_distance(Target, Current_Position, Heuristic).

calculate_heuristic(find(_), _Current_Position, 0).

calculate_heuristic(go(_Target, Target_Position), Current_Position, Heuristic) :-
	map_distance(Target_Position, Current_Position, Heuristic).


heuristic_sortable(go(_)).
heuristic_sortable(go(_,_)).
