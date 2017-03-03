% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  actors(Actors),
  filter_until_one(Actors, A).

filter_until_one([], _) :-
  !,
  fail.

filter_until_one([A], A) :-
  !.

filter_until_one(Actors, A) :-
  agent_ask_oracle(oscar, o(1), link, Link),
  filter_actors(Link, Actors, Filtered_Actors),
  filter_until_one(Filtered_Actors, A).
