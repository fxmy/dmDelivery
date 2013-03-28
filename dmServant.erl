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
	ServantPid ! {login, Group, Nick},
	
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
			%% {loginOK, OnlineNum, [OnlineLit]}
			%% then handles by ws_handler's OnInit
			GroupListNew = [Nick | GroupList],

