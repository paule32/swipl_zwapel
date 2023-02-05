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

%% ---------------------------------------------------------------------------
%% this section, the user have to adjust it to the need's:
%% change: appLang(X) :- english(X). for English locale
%% change: appLang(X) :- german(X).  for German  locale
%% ---------------------------------------------------------------------------
appLang(X) :- german(X).

english(X) :- X = 1.
german(X)  :- X = 2.

%% ---------------------------------------------------------------------------
%% library loading stuff, for use in this script tree.
%% ---------------------------------------------------------------------------
:- use_module(library(strings)).
:- use_module(library(lists)).
:- use_module(library(process)).

%% ---------------------------------------------------------------------------
%% Entry Point init. jump
%% ---------------------------------------------------------------------------
:- initialization(main, main).

%% ---------------------------------------------------------------------------
%% display a localizated message for information, warning, error, or debug ...
%% ---------------------------------------------------------------------------
loc_message(HA,HB,X) :-
	(HA == X -> write(HB);!).
%% ---------------------------------------------------------------------------
%% X, and HA = message number, HB = message text.
%% ---------------------------------------------------------------------------
loc_message([(HA,HB)|T],X) :-
	loc_message(HA,HB,X),true,!,
	loc_message(T,X).
%% ---------------------------------------------------------------------------
%% for empty message lists, do nothing.
%% ---------------------------------------------------------------------------
loc_message([],_).
loc_message(X) :-
	%% ----------------------------------------------------------
	%% get supported language lists for the application messages.
	%% currently only: English, and German.
	%% ----------------------------------------------------------
	englishList(EnglishList),	% English.msg
	germanList(GermanList), 	% German.msg
	appLang(AppLang),
	%% ----------------------------------------------------------
	%% 1 = English, 2 = German.
	%% ----------------------------------------------------------
	(AppLang == 1 -> loc_message(EnglishList,X);
	(AppLang == 2 -> loc_message(GermanList,X))),!.

%% ---------------------------------------------------------------------------
%% code of conduct: please let this notes intact, and unchanged - thank's !
%% ---------------------------------------------------------------------------
banner :-
	writeln("protus 1.0.0"),
	writeln("(c) 2023 by Jens Kallup - paule32"),
	writeln("").

%% ---------------------------------------------------------------------------
%% entry point execution ...
%% ---------------------------------------------------------------------------
main(Argv) :-	
	%% ---------------------------------------
	%% write a code of conduct notice:
	%% ---------------------------------------
	banner,
	%% ---------------------------------------
	%% load localization message files ...
	%% ---------------------------------------
	consult("English.msg"),
	consult("German.msg"),
	%% ---------------------------------------
	%% get application arguments ...
	%% ---------------------------------------
	maplist(=,Argv,ListArgv),
	%% ---------------------------------------
	%% if no argument given, error notice ...
	%% ---------------------------------------
	(Argv == [] -> err_NoInputFile(E),loc_message(E),halt;!),
	nth0(0,ListArgv,Argument0),
	%% ---------------------------------------
	%% at the current time, only argv: 1
	%% ---------------------------------------
	(exists_file(Argument0) -> !;
	err_CanNotOpen(E),loc_message(E),format(": ~q",[Argument0]), halt),
	writeln("parse file...")
	.
