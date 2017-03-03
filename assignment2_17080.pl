candidate_number(17080).

solve_task(Task,Cost) :-
  (   part(1) -> solve_task_1_3(Task, Cost)
  ;   part(3) -> solve_task_1_3(Task, Cost)
  ;   part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_1_3(-Task, +Cost)
solve_task_1_3(Task, [cost(Final_Cost), depth(Final_Depth)]) :-
  agent_current_position(oscar, Start_Position),
  Start_Cost = 0,
  Start_Depth = 0,
  calculate_heuristic(Task, Start_Position, Start_Heuristic),
  Start_Path = path(Start_Cost, Start_Depth, Start_Heuristic, [Start_Position]),
  insert_visited_position(Start_Position, Start_Cost, costs{}, Start_Costs),
  solve_task_bfs(
      Task,
      Start_Costs,
      [Start_Path],
      path(Final_Cost, Final_Depth, _Final_Heuristic, Final_Reverse_Path)
  ),
  !, % Once we've found a solution, we hope that it's the best one
  reverse(Final_Reverse_Path, [_Start_Position|Final_Path]),
  agent_do_moves(oscar, Final_Path),
  !. % we're done, prune any remaing choice points
%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_bfs(+Task, +Current_Paths, -Final_Path)
solve_task_bfs(Task, _Visited_Positions, [Current_Path|_Rest], Current_Path) :-
  achieved(Task, Current_Path).

solve_task_bfs(Task, Current_Visited_Positions, [path(Current_Cost, Current_Depth, _Current_Heuristic, Current_Path)|Rest], Final_Path) :-
  Current_Path = [Current_Position|_],
  empty_map_adjacent(Current_Position, Adjacent_Positions),
  prepend_to_paths(Current_Path, Adjacent_Positions, New_Paths),
  construct_paths(Task, Current_Cost, Current_Depth, New_Paths, New_Path_Functors),
  filter_positions(Current_Visited_Positions, New_Path_Functors, Filtered_New_Path_Functors),
  insert_previous_paths(Filtered_New_Path_Functors, Current_Visited_Positions, New_Visited_Positions),
  append(Rest, Filtered_New_Path_Functors, New_Agenda),
  (   heuristic_sortable(Task)
  ->  sort(3, @=<, New_Agenda, Sorted_New_Agenda)
  ;   Sorted_New_Agenda = New_Agenda),
  solve_task_bfs(Task, New_Visited_Positions, Sorted_New_Agenda, Final_Path).


% achieved(+Task, +Current_Path)
achieved(go(Target), path(_Final_Cost, _Final_Depth, _Final_Heuristic, [Target|_])).

achieved(find(Target), path(_Final_Cost, _Final_Depth, _Final_Heuristic, [Final_Position|_])) :-
	map_adjacent(Final_Position, _, Target).

% empty_map_adjacent(+Current_Position, -All_Empty_Adjacent_Positions)
empty_map_adjacent(Current_Position, All_Empty_Adjacent_Positions) :-
  empty_map_adjacent_acc(Current_Position, [], All_Empty_Adjacent_Positions).

% empty_map_adjacent_acc(+Current_Position, +Current_Adjacent_Positions,
% -All_Empty_Adjacent_Positions)
empty_map_adjacent_acc(Current_Position, Current_Adjacent_Positions, All_Adjacent_Positions) :-
  map_adjacent(Current_Position, New_Adjacent_Position, empty),
  \+ memberchk(New_Adjacent_Position, Current_Adjacent_Positions),
  !,
  empty_map_adjacent_acc(Current_Position, [New_Adjacent_Position|Current_Adjacent_Positions], All_Adjacent_Positions).

empty_map_adjacent_acc(_Current_Position, All_Adjacent_Positions, All_Adjacent_Positions).


% filter_positions(+Visited_Positions, +Positions,
% -All_Filtered_Positions)
filter_positions(_Visited_Positions, [], []).

filter_positions(Visited_Positions, [Path|Paths], All_Filtered_Paths) :-
  (   better_path_found(Visited_Positions, Path)
  ->  All_Filtered_Paths = Filtered_Paths
  ;   All_Filtered_Paths = [Path|Filtered_Paths]
  ),
  filter_positions(Visited_Positions, Paths, Filtered_Paths).


better_path_found(Visited_Positions, path(Cost, _Depth, _Heuristic, [Position|_])) :-
  contains_visited_position(Visited_Positions, Position, Visited_Cost),
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

heuristic_sortable(go(_)).

contains_visited_position(Existing_Costs, p(X, Y), Cost) :-
  get_dict(Y, Existing_Costs, Existing_Row),
  get_dict(X, Existing_Row, Cost).


insert_previous_paths([], Existing_Costs, Existing_Costs).

insert_previous_paths([path(Cost, _Depth, _Heuristic, [Position|_])|Paths], Old_Existing_Costs, New_Existing_Costs) :-
  insert_visited_position(Position, Cost, Old_Existing_Costs, Current_Existing_Costs),
  insert_previous_paths(Paths, Current_Existing_Costs, New_Existing_Costs).


insert_visited_position(p(X, Y), Cost, Existing_Costs, New_Costs) :-
  (   get_dict(Y, Existing_Costs, Existing_Row)
  ->  true
  ;   Existing_Row = costs{}
  ),
  put_dict(X, Existing_Row, Cost, New_Row),
  put_dict(Y, Existing_Costs, New_Row, New_Costs).

scan_map :-
  between(1, 20, Y),
  between(1, 20, X),
  map_adjacent(p(X, Y), _, _),
  false.

actors(Actors) :-
	actors([], Actors).

actors(Current_Actors, All_Actors) :-
	actor(Actor),
	\+ memberchk(Actor, Current_Actors),
	!,
	actors([Actor|Current_Actors], All_Actors).

actors(Actors, Actors).

actor_with_all_links(_Actor, []).

actor_with_all_links(Actor, [Link|Links]) :-
	wp(Actor, WikiText),
	wt_link(WikiText, Link),
	actor_with_all_links(Actor, Links).

filter_actors(_Links, [], []).

filter_actors(Links, [Actor|Actors], All_Filtered_Actors) :-
	(   actor_with_all_links(Actor, Links)
	->  All_Filtered_Actors = [Actor|Filtered_Actors]
	;   All_Filtered_Actors = Filtered_Actors),
	!,
	filter_actors(Links, Actors, Filtered_Actors).

