-module(dmMaster).
-behavior(application).


-export([start/2,stop/1]).

%% dmMaster tracks many dmServants
%% handles the very first login message of chat users in a same chat group( that is served in a same dmServant)
%%
%% when a group's OnlineNum goes zero, sends kill signal to the conresponding dmServant
%%


start( _Type, _Args) ->
	%% start the dmMaster.
	%% regiter as dmMaster
	%% trap_exit => true to track those dmServants and restart them should they crash
	%% initialize a Set of {DmServantName, {DmServantPid, OnlineNum}} to hold dmServants' info
	%% then start_links with supervisor by calling dmMaster_sup:start_link
	%% and finally goes into the main loop
	register(dmMaster, self()),
	ServantSet = ets:new( dmMasterSet, [ordered_set]),
	process_flag( trap_exit, true),
	dmMaster_sup:start_link(),
	loop(ServantSet).



stop() ->
	ok.



loop( ServantSet) ->
	%% handles {login,Group,Nick,Pid} signal from client
	%% and {clientExit, OnlineNum, Pid} signal from dmServant
	%% should also handle dmServant's unnormal EXIT signal
	receive
		{login, Group, Nick, From} ->
			%% client attempt to login, lookup ServantSet for the specified Group
			%% if exits, relay the signal to the particular dmServant
			%% if not, call dmServant:start( Group, Nick) to spwan a new dmServant
			%% 	and then relay the signal

