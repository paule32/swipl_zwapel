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
appLang(X) :- english(X).

english(X) :- X = 1.
german(X)  :- X = 2.

%% ---------------------------------------------------------------------------
%% library loading stuff, for use in this script tree.
%% ---------------------------------------------------------------------------
:- assertz(file_search_path(library,pce('prolog/lib'))).
:- assertz(file_search_path(pce,'e:/swipl/xpce')).

:- use_module(library(pce)).
:- use_module(library(pce_config)).
:- use_module(library(pce_report)).
:- use_module(library(toolbar)).

:- use_module(library(strings)).
:- use_module(library(lists)).
:- use_module(library(process)).

:- ensure_loaded(library(help_message)).

%% ---------------------------------------------------------------------------
%% gui stuff ...
%% ---------------------------------------------------------------------------
:- pce_autoload(report_dialog,library(pce_report)).
:- pce_autoload(tool_bar,library(toolbar)).
:- pce_autoload(finder,library(find_file)).
:- pce_global(@finder,new(finder)).

resource(printer, image, image('16x16/print.xpm')).
resource(floppy,  image, image('16x16/save.xpm')).

:- set_prolog_flag(report_error,true).
:- set_prolog_flag(unknown,error).		%% for unknown predicates

%% ---------------------------------------------------------------------------
%% Entry Point init. jump
%% ---------------------------------------------------------------------------
:- initialization(main, main).

%% ---------------------------------------------------------------------------
%% display a localizated message for information, warning, error, or debug ...
%% ---------------------------------------------------------------------------
loc_message(HA,HB,X) :-
	(HA == X -> writeln(HB);!).

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
	messageList(MsgList),
	loc_message(MsgList,X).

%% ---------------------------------------------------------------------------
%& error handling messages ...
%% ---------------------------------------------------------------------------
emitError(X,Flag) :- Flag = true, writeln(X).
catchError(G) :-
	E = error(existence_error(procedure,F),_),
		catch(G,E,emitError(F,Flag)),
			(Flag == true -> writeln("falserer"),!;
	E = error(syntax_error(Id),_),
		catch(G,E,emitError(Id,Flag)),
			(Flag == true -> writeln("fuuoos")
		)).

foodid(X) :- writeln(X).
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
	catchError(foodid(_,2)), %error(Err,_Context),write(Err)),

	%% ---------------------------------------
	%% write a code of conduct notice:
	%% ---------------------------------------
%%	foodid(2),
	banner,

	%% ----------------------------------------------------------
	%% for each session load localization message file ...
	%% 1 = English, 2 = German.
	%% ----------------------------------------------------------
	appLang(AppLang),
	(AppLang == 1 -> consult('English.msg');
	(AppLang == 2 -> consult('German.msg'))),!,

	%% ---------------------------------------
	%% get application arguments ...
	%% ---------------------------------------
	maplist(=,Argv,ListArgv),
	%% ---------------------------------------
	%% if no argument given, error notice ...
	%% ---------------------------------------
	(Argv == [] -> err_NoInputFile(E),loc_message(E),halt;!),
	nth0(0,ListArgv,Argument0),

	new(Application,myapp),
	send(Application,destroy),
	writeln('done'),
	
	%% ---------------------------------------
	%% at the current time, only argv: 1
	%% ---------------------------------------
	(exists_file(Argument0) -> !;
	err_CanNotOpen(E),loc_message(E),format(": ~q",[Argument0]), halt),
	writeln("parse file...")
	.

%% ---------------------------------------------------------------------------
%% @brief class that describe our application ...
%% ---------------------------------------------------------------------------
:- pce_begin_class(myapp,frame,"Frame").

initialise(MyApp) :->
	programTitle(AppTitle),
	send_super(MyApp,initialise,AppTitle),
	send(MyApp,append,new(D,dialog)),
	
	send(D,append,new(menu_bar)),
	send(D,append,new(tool_bar(MyApp))),
	
	send(MyApp,fill_menu_bar),
	send(MyApp,fill_tool_bar),
	
	send(new(W,myapp_workspace),below,D),
	send(W,scrollbars,both),
	send(W,size,size(640,480)),
	send(new(report_dialog),below,W),
	
	get(MyApp,confirm_centered,@nil).

%% ---------------------------------------------------------------------------
%% menubar stuff ...
%% ---------------------------------------------------------------------------
fill_menu_bar(F) :->
	get(F,member,dialog,D),
	get(D,member,menu_bar,MB),
	
	menuFile_Text(FileText),
	menuHelp_Text(HelpText),
	menuExitApp_Text(ExitAppText),
	
	send_list(MB,append,
		[	new(File, popup(FileText)),
			new(Help, popup(HelpText))
		]),	
	send_list(File,append,
		[	menu_item(destroyApp,message(F,destroyApp),ExitAppText)
		]).

%% ---------------------------------------------------------------------------
%% toolbar stuff ...
%% ---------------------------------------------------------------------------
fill_tool_bar(F) :->
	get(F,member,dialog,D),
	get(D,member,tool_bar,TB),
	
	toolBarSave_Text(TBSave_Text),
	toolBarSave_2_Text(TBSave_2_Text),

	send_list(TB,append,
		[	tool_button(destroyApp,image('16x16/save.xpm'),TBSave_Text),
			tool_button(destroyApp,image('16x16/save.xpm'),TBSave_2_Text)
		]).

%% ---------------------------------------------------------------------------
%% destroy the current class ...
%% ---------------------------------------------------------------------------
destroyApp(F) :->
	get(F,member,dialog,D),
	send(D,destroy).

:- pce_end_class(myapp).


:- pce_begin_class(myapp_workspace, window).
:- pce_end_class(myapp_workspace).
