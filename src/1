%% ---------------------------------------------------------------------------
%% Entry Point init.
%% ---------------------------------------------------------------------------
:- initialization(main, main).

string1("dummy").
string2("dummy").

verb_liste(["ist","auf","laufen"]).

verb(X) :-
	verb_liste(MyList),
	member(X,MyList).

ist_verb(X) :- verb(X).

eingabe(Prompt, Antwort) :-
	prompt1(Prompt),
	read_string(current_input, "\n", "\t", _Sep, Eingabe),
	bestätigung(Eingabe, Antwort), !.

bestätigung(EingabeText, Antwort) :-
	B = ".exit",
	string_lower(EingabeText, R),
	%% --------------------------
	%% compare two string's ...
	%% --------------------------
	(   R == B
	->  writeln("vielen Dank für die Benutzung."),halt
	;   verb_liste(VerbenListe),
	    (   memberchk(R, VerbenListe)
	    ->  (   ist_verb(R)
	        ->  Antwort = "ist ein Verb"
	        ;   Antwort = "ist kein Verb"
	        )
	    ;       Antwort = "nicht gefunden."
	    )
	).

%% ---------------------------------------------------------------------------
%% Entry Point start-up
%% ---------------------------------------------------------------------------
main :-
	write("Bitte Text eingeben: "),
	eingabe("", Antwort),
	writeln(Antwort),
	main.
