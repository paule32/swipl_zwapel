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

err_NoInputFile(X) :- X = 4.
err_CanNotOpen(X)  :- X = 5.

germanList([
	(1,"Hallo"),
	(2,"Welt"),
	(3,"Traum"),
	(4,"keine Eingabe-Datei angegeven. FAIL"),
	(5,"kann Datei nicht öffnen")
]).
