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
%% program information stuff ...
%% ---------------------------------------------------------------------------
programTitle(X)   :- X = "My Application".
programVersion(X) := X = "1.0.0".

%% ---------------------------------------------------------------------------
%% user menu-bar items ...
%% ---------------------------------------------------------------------------
menuFile_Text(X)    :- X = "File".
menuHelp_Text(X)    :- X = "Help".
menuExitApp_Text(X) :- X = "Exit Application".

%% ---------------------------------------------------------------------------
%% user tool-bar items ...
%% ---------------------------------------------------------------------------
toolBarSave_Text(X)   :- X = "Save".
toolBarSave_2_Text(X) :- X = "Sava as ...".

%% ---------------------------------------------------------------------------
%% shortner/alias ...
%% ---------------------------------------------------------------------------
err_NoInputFile(X) :- X = 4.
err_CanNotOpen(X)  :- X = 5.

messageList([
	(1, "Hello"),
	(2, "World"),
	(3, "Dream"),
	(4, "no input file"),
	(5, "file could not open")
]).
