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
