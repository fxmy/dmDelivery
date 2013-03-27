-module(dmServant).
-export([start/2, stop/1]).

%% called by dmMaster to start a brand new dmServant for a new chat channel
%% holds a List of online users
%%
%%

start( Group, Nick) ->
	ServantPid = spawn_link( fun loop/1),
	register( Group, ServantPid),

	%% sends the FIRST logged user
	ServantPid ! { Group, Nick},
	
	ServantPid.

%% loop/1 does the real work
%% receives message from the user and dispatch to others
%% 
%% receives & responses command form dmMaster
%%
%% when the user list goes empty( means all users exited), atomatically exit self
