-module(dmServant).
-export([start/4, stop/1, reboot/1,loop/1]).
%-export([getOnlineUser/2,dispatchChat/2,dispacthOnlineUser/2]).

%% called by dmMaster to start a brand new dmServant for a new chat channel
%% holds a List of online users
%%
%%

start( Group, Nick, From, PreserverPid) ->
	ServantPid = spawn_link( dmServant, loop, [dontCare]),
	register( Group, ServantPid),

	%% set up a TABLE to hold logged clients, then give_away to ServantPid
	%% set the TABLE's heir to the PreserverPid
	TabID = ets:new(Group,[set,{heir, PreserverPid, Group}]),
	ets:give_away(TabID, ServantPid, []),

	%% sends the FIRST logged user
	ServantPid ! {login, Nick, From},
	
	ServantPid.

stop(_State) ->
	true.

reboot(Group) ->
	ServantPid = spawn_link( dmServant,loop,[dontCare]),
	register(Group, ServantPid),
	ServantPid.


%% loop/1 does the real work
%% receives message from the user and dispatch to others
%% 
%% receives & responses command form dmMaster
%%
%% when the user list goes empty( means all users exited), dmMaster will send a msg thus stop the Servant

loop( GroupTable) ->
	receive
		{'ETS-TRANSFER',TabID, _FromPid, GiftData} ->
			%% sended from master for initialization
			%% or from the dmClientPreserver for clients preserver
			%% on both condition, dmServant just need to take the new table
			io:format("INFO:ETS-TRANSFER,channel ~p~n",[GiftData]),
			loop(TabID);

		{login, Nick, From} ->
			%% sends from ws_handler:OnInit, forwarded by dmMaster
			%% responses to ws_handler's PID of 
			%% {loginOK, OnlineNum, [OnlineList]}
			%% then handles by ws_handler's OnInit
			%GroupListNew = [ {Nick,From} | GroupList],
			%OnlineNum = erlang:length(GroupListNew),
			%From ! {loginOK, OnlineNum, GroupListNew},
			io:format("logged in : Nick,From ~p ~p~n", [Nick, pid_to_list(From)]),
			ets:insert(GroupTable, {Nick, From}),
			OnlineNum = ets:info( GroupTable, size),
			io:format("size ~p~n",[OnlineNum]),
			OnlineUser = ets:foldl( fun getOnlineUser/2, [], GroupTable),
			io:format("OnlineUser: ~p~n",[OnlineUser]),
			From ! loginOK,
			ets:foldl(fun dispatchOnlineUser/2,{OnlineNum,OnlineUser},GroupTable),
			loop( GroupTable);

		{chat, Nick, Text, _From} ->
			%% msgs from clients handled here
			%% dispatch Text to every member in GroupList
			%lists:foreach( fun({Nick,From})->
			%			From ! {chatUpdate, Nick, Text}
			%	end,
			%	GroupList),
			ets:foldl( fun dispatchChat/2, {Nick,Text}, GroupTable),
			loop( GroupTable);

		{logout, Nick, From} ->
			%% received from clients
			%% remove the client from GroupList
			%% then notify dmMaster the new OnlineNum
			%GroupListNew = lists:delete({Nick, From}),
			ets:delete_object( GroupTable, {Nick,From}),
			%OnlineNumNew = erlang:length(GroupListNew),
			OnlineNumNew = ets:info( GroupTable, size),
			dmMaster ! {clientExit, OnlineNumNew, self()},
			OnlineUser = ets:foldl( fun getOnlineUser/2, [], GroupTable),
			ets:foldl(dispatchOnlineUser,OnlineUser,GroupTable),
			loop( GroupTable);
		
		%% everybody leaves,
		%% dmMaster sends kill signal
		endServant ->
			true
end.


%% get online user list fun
getOnlineUser({Nick, _Pid}, OnlineUser) ->
	[Nick|OnlineUser].

%% dispatch the chat
dispatchChat({_Nick, Pid}, {Nick, Text}) ->
	Pid ! {chatUpdate, Nick, Text},
	{Nick, Text}.

dispatchOnlineUser( {_Nick,Pid}, {OnlineNum,OnlineUser}) ->
	Pid ! {onLineUserUpdate,OnlineNum,OnlineUser},
	{OnlineNum,OnlineUser}.
