%% ---------------------------------------------------------------------------
%% Part of Prolog Test
%%
%% Author:        Jens Kallup - paule32
%% E-mail:        paule32.jk@gmail.com
%% WWW:           http://
%% Copyright (c)  2005-2023, DIAKO West-Thueringen
%%                           kallup non-profit
%% All rights reserved.
%% ---------------------------------------------------------------------------

:- use_module(library(lists)).
:- use_module(library(error)).

%% ---------------------------------------------------------------------------
%% Entry Point init.
%% ---------------------------------------------------------------------------
:- initialization(main, main).

%% ---------------------------------------------------------------------------
%% Konnektoren:
%% ---------------------------------------------------------------------------
:- op(500,fy,   'not').  %% negation     NOT     - nicht
:- op(1000,xfy, 'and').  %% conjuntion   AND     - und
:- op(1100,xfy,  'or').  %% disjunktion  OR      - oder
:- op(1110,xfy,'when').  %% implication  IF THEN - wenn ... dann ...
:- op(1120,xfy,'then').  %% implication  IF THEN - ...

%% ---------------------------------------------------------------------------
%% Wahrheits Tabelle: AND
%% ---------------------------------------------------------------------------
conjunction(1,1,1).
conjunction(1,0,0).
conjunction(0,1,0).
conjunction(0,0,0).

%% ---------------------------------------------------------------------------
%% Wahrheits Tabelle: OR
%% ---------------------------------------------------------------------------
disjunction(1,1,1).
disjunction(1,0,1).
disjunction(0,1,1).
disjunction(0,0,0).

%% ---------------------------------------------------------------------------
%% Wahrheits Tabelle: NOT
%% ---------------------------------------------------------------------------
negation(1,0).
negation(0,1).

%% ---------------------------------------------------------------------------
%% Boolean constants in expression
%% ---------------------------------------------------------------------------
find_vars(N,V,V) :- member(N,[0,1]),!.

find_vars(X,Vin,Vout) :-
    atom(X), 
    (member(X,Vin) -> Vout = Vin;
                      Vout = [X|Vin]).

find_vars(X then Y,Vin,Vout) :- find_vars(X,Vin,Vtemp),
                               find_vars(Y,Vtemp,Vout).

find_vars(X when Y,Vin,Vout)  :- find_vars(X,Vin,Vtemp),
                               find_vars(Y,Vtemp,Vout).

find_vars(X and Y,Vin,Vout)   :- find_vars(X,Vin,Vtemp),
                               find_vars(Y,Vtemp,Vout).

find_vars(X or  Y,Vin,Vout)  :- find_vars(X,Vin,Vtemp),
                               find_vars(Y,Vtemp,Vout).

find_vars(not X,Vin,Vout)     :- find_vars(X,Vin,Vout).

next([1|R],[0|R]).
next([0|R],[1|S]) :- next(R,S).

%% ---------------------------------------------------------------------------
%% initial_assign 1 and not 0.
%% ---------------------------------------------------------------------------
initial_assign([],[]).
initial_assign([_|R],[1|S]) :- initial_assign(R,S).

%% ---------------------------------------------------------------------------
% predecessor instead of successor.
%% ---------------------------------------------------------------------------
predecessor(A,S) :- reverse(A,R),
                    next(R,N),
                    reverse(N,S).

truth_value(N,_,_,N)      :- member(N,[0,1]).
truth_value(X,Vars,A,Val) :- atom(X),
                             lookup(X,Vars,A,Val).

truth_value(X then Y,Vars,A,Val) :- truth_value(X,Vars,A,VX),
                                   truth_value(Y,Vars,A,VY),
                                   biconditional(VX,VY,Val).

truth_value(X when Y,Vars,A,Val)  :- truth_value(X,Vars,A,VX),
                                   truth_value(Y,Vars,A,VY),
                                   conditional(VX,VY,Val).
								  
truth_value(X and Y,Vars,A,Val)   :- truth_value(X,Vars,A,VX),
                                   truth_value(Y,Vars,A,VY),
                                   conjunction(VX,VY,Val).

truth_value(X or Y,Vars,A,Val)   :- truth_value(X,Vars,A,VX),
                                   truth_value(Y,Vars,A,VY),
                                   disjunction(VX,VY,Val).

truth_value(not X,Vars,A,Val)      :- truth_value(X,Vars,A,VX),
                                   negation(VX,Val).

lookup(X,[X|_],[V|_],V).
lookup(X,[_|Vars],[_|A],V) :- lookup(X,Vars,A,V).

tt(E) :- find_vars(E,[],V),
         reverse(V,Vars),
         initial_assign(Vars,A),
         format('  '), write(Vars),format('   '),write(E), nl,
         format('-----------------------------------------'), nl,
         write_row(E,Vars,A),
         format('-----------------------------------------'), nl.

write_row(E,Vars,A) :- format('  '), write(A),format('     '), 
                       truth_value(E,Vars,A,V),write(V),nl,
                       (predecessor(A,N) -> write_row(E,Vars,N)
		       ; true).

%% :- tt(x or (not y  and z)).


/*
mensch(menti).
mensch(jens).

frage_essen(katzen, X) :- mensch(X), !, fail.
frage_essen(katzen, fish).
frage_essen(katzen, jens).

essen(X, Y) :-
    once(frage_essen(X, Y))
    ->  writef('%t essen %t\n', [X,Y])
    ;   writef('%t essen keine %t\n', [X,Y]).


eol --> "\n", !.
eol --> "\r\n", !.
eol --> eos.

  */

echo([])     :- nl.
echo([Last]) :- !,
    write(Last), nl.
echo([H|T])  :-
    write(H), write(' '),
    echo(T).

main(Argv) :-
    tt(x or (not y  and z)),
    echo(Argv).
