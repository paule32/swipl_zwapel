:- use_module(library(lists)).
:- use_module(library(error)).

%% ---------------------------------------------------------------------------
%% Entry Point init.
%% ---------------------------------------------------------------------------
:- initialization(main, main).

verb_liste(['ist','auf','laufen']).

verb('auf').
verb('laufen').

verb([]) :-
	 write("nicht in Liste !").
verb(X)  :-
	 string(X),
	 atom_string(A,X),
	 verb(A).
verb(X)  :-
	 verb_liste(mylist),
	 atom(X),
	 (member(X,[mylist]) -> true; false).

ist_verb(X) :-
	(verb(X) -> write(X), write(" ist ein Verb.")).

main :-
	atom_string("Haus",DEU_Haus),
	atom_string("Maus",DEU_Maus),
	atom_string("Sachsen",DEU_Sachsen),
	atom_string("Bremen",DEU_Bremen),
	Deutschland_Länder = [DEU_Haus,DEU_Maus,DEU_Sachsen,DEU_Bremen],
	(member("Bremen",Deutschland_Länder) -> write("ok") ; write("false")),nl,
	ist_verb('ist').
