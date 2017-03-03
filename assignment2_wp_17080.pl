% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(Identity) :-
  actors(Actors),
  ask_next_oracle(Actors, Identity).

ask_next_oracle([], _Our_Identity) :-
  !,
  format("No Identities Remaining!\n"),
  fail.

ask_next_oracle([Our_Identity], Our_Identity) :-
  format("Identity Found: ~w\n", [Our_Identity]),
  !.

ask_next_oracle(Remaining_Actors, Our_Identity) :-
  writeln("Looking for an oracle"),
  agent_current_position(oscar, Start_Position),
  agent_current_energy(oscar, Current_Energy),
  find_path(find(o(O_N)), Start_Position, Oracle_Position, Oracle_Path, Oracle_Cost, _Oracle_Depth),
  \+ agent_check_oracle(oscar, o(O_N)),
  (   find_path(find(c(O_C_N)), Oracle_Position, _Oracle_Charge_Position, _Oracle_Charge_Path, Oracle_Charge_Cost, _Oracle_Charge_Depth),
      format("Can we reach ~w then ~w?\n", [o(O_N), c(O_C_N)]),
      Total_Cost is Oracle_Cost + Oracle_Charge_Cost + 10,
      format("Energy: ~w\nCost: ~w\n", [Current_Energy, Total_Cost]),
      (   Total_Cost < Current_Energy
      ->  !,
          format("PASS\n", []),
          go_to_oracle_and_ask(o(O_N), Oracle_Path, Remaining_Actors, Filtered_Actors),
          ask_next_oracle(Filtered_Actors, Our_Identity)
      ;   format("FAIL\n", []),
          find_path(find(c(C_N)), Start_Position, _Charge_Position, Charge_Path, _Charge_Cost, _Charge_Depth),
          !,
          format("Going to ~w to refuel\n", [c(C_N)]),
          agent_do_moves(oscar, Charge_Path),
          agent_topup_energy(oscar, c(C_N)),
          !,
          ask_next_oracle(Remaining_Actors, Our_Identity))
  ;   format("Can't find a refuelling station, going to ~w anyway\n", [o(O_N)]),
      go_to_oracle_and_ask(o(O_N), Oracle_Path, Remaining_Actors, Filtered_Actors),
      ask_next_oracle(Filtered_Actors, Our_Identity)).

go_to_oracle_and_ask(Oracle, Oracle_Path, Remaining_Actors, Filtered_Actors) :-
  agent_do_moves(oscar, Oracle_Path),
  !,
  agent_ask_oracle(oscar, Oracle, link, Link),
  format("Current Actors: ~w\nLink: ~w\n", [Remaining_Actors, Link]),
  filter_actors(Link, Remaining_Actors, Filtered_Actors),
  format("Filtered_Actors: ~w\n", [Filtered_Actors]),
  !.

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
