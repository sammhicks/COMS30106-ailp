candidate_number(17080).

solve_task(Task,Cost) :-
  (   part(1) -> solve_task_1_3(Task, Cost)
  ;   part(3) -> solve_task_1_3(Task, Cost)
  ;   part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_1_3(-Task, +Cost)
solve_task_1_3(Task, Cost) :-
  agent_current_position(oscar, Start_Position),
  solve_task_bt(Task,[c(0, Start_Position), Start_Position], 0, R, Cost, _NewPos),
  !, % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  agent_do_moves(oscar,Path),
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
  search(Task, Current_Position, Adjacent_Position, Adjacent_Position, Move_Cost), % Search for a suitable adjacent position
  \+ memberchk(Adjacent_Position, Reverse_Path),  % check we have not been here already
  % Calculate the new state
  New_Depth is Current_Depth + 1,
  New_Cost is Current_Cost + Move_Cost,
  New_Path = [c(New_Cost, Adjacent_Position), Adjacent_Position|Reverse_Path],
  % Solve using the new state
  solve_task_bt(Task, New_Path, New_Depth, Final_Reverse_Path, Total_Cost, New_Position).


% achieved(+Task, +Current_Path, -Reverse_Path, -Total_Cost,
% -New_Position)
achieved(go(Exit), Current_Path, Reverse_Path, Total_Cost, New_Position) :-
  Current_Path = [c(Total_Cost, New_Position)|Reverse_Path],
  (   Exit = none -> true
  ;   otherwise -> Reverse_Path = [Exit|_]
  ).

achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

% search(+Target_Position, +Current_Position, -Adjacent_Position,
% -Adjacent_Position, -Cost)
search(go(Target_Position), Current_Position, Adjacent_Position, Adjacent_Position, 1) :-
  empty_map_adjacent(Current_Position, Adjacent_Positions), % Find all empty adjacent cells
  position_distances(Target_Position, Adjacent_Positions, Position_Distances), % Calculate their distances from the target
  sort(2, @=<, Position_Distances, Sorted_Position_Distances), % Sort them based on their distances
  member(pd(Adjacent_Position, _Distance), Sorted_Position_Distances). % Return the cells, starting with the smallest


% empty_map_adjacent(+Current_Position, -All_Empty_Adjacent_Positions)
empty_map_adjacent(Current_Position, All_Empty_Adjacent_Positions) :-
  empty_map_adjacent(Current_Position, [], All_Empty_Adjacent_Positions).

% empty_map_adjacent(+Current_Position, +Current_Adjacent_Positions,
% -All_Empty_Adjacent_Positions)
empty_map_adjacent(Current_Position, Current_Adjacent_Positions, All_Adjacent_Positions) :-
  map_adjacent(Current_Position, New_Adjacent_Position, empty),
  \+ memberchk(New_Adjacent_Position, Current_Adjacent_Positions),
  !,
  empty_map_adjacent(Current_Position, [New_Adjacent_Position|Current_Adjacent_Positions], All_Adjacent_Positions).

empty_map_adjacent(_Current_Position, All_Adjacent_Positions, All_Adjacent_Positions).


% position_distances(+Target, +Positions, -Position_Distances)
position_distances(_, [], []).

position_distances(Target, [Position|Positions], [pd(Position, Distance)|Position_Distances]) :-
  map_distance(Target, Position, Distance),
  position_distances(Target, Positions, Position_Distances).
