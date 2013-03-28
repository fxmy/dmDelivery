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
	ServantPid ! {login, Nick, From},
	
	ServantPid.

%% loop/1 does the real work
%% receives message from the user and dispatch to others
%% 
%% receives & responses command form dmMaster
%%
%% when the user list goes empty( means all users exited), dmMaster will send a msg thus stop the Servant

loop( GroupList) ->
	receive
		{login, Nick, From} ->
			%% sends from ws_handler:OnInit, forwarded by dmMaster
			%% responses to ws_handler's PID of 
			%% {loginOK, OnlineNum, [OnlineList]}
			%% then handles by ws_handler's OnInit
			GroupListNew = [ {Nick,From} | GroupList],
			OnlineNum = erlang:length(GroupListNew),
			From ! {loginOK, OnlineNum, GroupListNew},
			loop( GroupListNew);

		{chat, Nick, Text, From} ->
			%% msgs from clients handled here
			%% dispatch Text to every member in GroupList
			lists:foreach( fun({Nick,From})->
						From ! {chatUpdate, Nick, Text}
				end,
				GroupList),
			loop( GroupList);

		{logout, Nick, From} ->
			%% received from clients
			%% remove the client from GroupList
			%% then notify dmMaster the new OnlineNum
			GroupListNew = lists:delete({Nick, From}),
			OnlineNumNew = erlang:length(GroupListNew),
			dmMaster ! {clientExit, OnlineNumNew, self()},
			loop( GroupListNew);
		
		%% everybody leaves,
		%% dmMaster sends kill signal
		{killServant} ->
			true
end.

