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
  solve_task_bfs(
      Task,
      [path(Start_Cost, Start_Depth, Start_Heuristic, [Start_Position])],
      path(Final_Cost, Final_Depth, _Final_Heuristic, Final_Reverse_Path)
  ),
  !, % Once we've found a solution, we know that it's the best one
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
%% backtracking depth-first search, needs to be changed to agenda-based A*
% solver_task_bt(+Task, +Current_Path, +Current_Depth, -Reverse_Path,
% -Costs, -New_Position)
solve_task_bt(Task, Current_Path, Current_Depth, Reverse_Path, [cost(Total_Cost), depth(Current_Depth)], New_Position) :-
  achieved(Task, Current_Path, Reverse_Path, Total_Cost, New_Position),
  !. % If we've achieved our task, stop.

solve_task_bt(Task, Current_Path, Current_Depth, Final_Reverse_Path, Total_Cost, New_Position) :-
  Current_Path = [c(Current_Cost, Current_Position)|Reverse_Path],
  search_bt(Task, Current_Position, Adjacent_Position, Adjacent_Position, Move_Cost), % Search for a suitable adjacent position
  \+ memberchk(Adjacent_Position, Reverse_Path),  % check we have not been here already
  % Calculate the new state
  New_Depth is Current_Depth + 1,
  New_Cost is Current_Cost + Move_Cost,
  New_Path = [c(New_Cost, Adjacent_Position), Adjacent_Position|Reverse_Path],
  % Solve using the new state
  solve_task_bt(Task, New_Path, New_Depth, Final_Reverse_Path, Total_Cost, New_Position).


% solve_task_bfs(+Task, +Current_Paths, -Final_Path)
solve_task_bfs(Task, [Current_Path|_Rest], Current_Path) :-
  achieved(Task, Current_Path).


solve_task_bfs(Task, [path(Current_Cost, Current_Depth, _Current_Heuristic, Current_Path)|Rest], Final_Path) :-
  Current_Path = [Current_Position|_],
  empty_map_adjacent(Current_Position, Adjacent_Positions),
  filter_positions(Current_Path, Adjacent_Positions, Filtered_Adjacent_Positions),
  prepend_to_paths(Current_Path, Filtered_Adjacent_Positions, New_Paths),
  construct_paths(Task, Current_Cost, Current_Depth, New_Paths, New_Path_Functors),
  append(Rest, New_Path_Functors, New_Agenda),
  sort(3, @=<, New_Agenda, Sorted_New_Agenda),
  solve_task_bfs(Task, Sorted_New_Agenda, Final_Path).


% achieved(+Task, +Current_Path)
achieved(go(Target), path(_Final_Cost, _Final_Depth, _Final_Heuristic, [Target|_])) :-
  (   ground(Target)
  ->  true
  ;   Target = none).

% TODO - Rewrite in our format
%achieved(find(O),Current,RPath,Cost,NewPos) :-
%  Current = [c(Cost,NewPos)|RPath],
%  ( O=none    -> true
%  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
%  ).

% search(+Target_Position, +Current_Position, -Adjacent_Position,
% -Adjacent_Position, -Cost)
search_bt(go(Target_Position), Current_Position, Adjacent_Position, Adjacent_Position, 1) :-
  empty_map_adjacent(Current_Position, Adjacent_Positions), % Find all empty adjacent cells
  position_distances(Target_Position, Adjacent_Positions, Position_Distances), % Calculate their distances from the target
  sort(2, @=<, Position_Distances, Sorted_Position_Distances), % Sort them based on their distances
  member(pd(Adjacent_Position, _Distance), Sorted_Position_Distances). % Return the cells, starting with the smallest


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


% filter_positions(+Disallowed_Positions, +Positions, -All_Filtered_Positions)
filter_positions(_Disallowed_Positions, [], []).

filter_positions(Disallowed_Positions, [Position|Positions], All_Filtered_Positions) :-
  (   memberchk(Position, Disallowed_Positions)
  ->  All_Filtered_Positions = Filtered_Positions
  ;   All_Filtered_Positions = [Position|Filtered_Positions]
  ),
  filter_positions(Disallowed_Positions, Positions, Filtered_Positions).


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

scan_map :-
  between(1, 20, Y),
  between(1, 20, X),
  map_adjacent(p(X, Y), _, _),
  false.
