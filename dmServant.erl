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
