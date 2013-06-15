-module(dmMaster).
-behavior(application).


-export([start/2,stop/1]).
-export([realstart/2]).

%% dmMaster tracks many dmServants
%% handles the very first login message of chat users in a same chat group( that is served in a same dmServant)
%%
%% when a group's OnlineNum goes zero, sends kill signal to the conresponding dmServant
%%
start( _Type,_Args) ->
	{ok,spawn( ?MODULE, realstart,[dontcare,dontcare])}.

realstart( _Type, _Args) ->
	%% start the dmMaster.
	%% regiter as dmMaster
	%% trap_exit => true to track those dmServants and restart them should they crash
	%% initialize a Set of 
	%% **{DmServantName, DmServantPid, OnlineNum}**
	%% 	to hold dmServants' info
	%% then start_links with supervisor by calling dmMaster_sup:start_link
	%% and finally goes into the main loop
	register(dmMaster, self()),
	ServantSet = ets:new( dmMasterSet, [set]),
	process_flag( trap_exit, true),
	Preserver = dmClientPreserver:start(),
	%dmMaster_sup:start_link(),
	loop({ServantSet, Preserver}).



stop(_State) ->
	ok.



loop( {ServantSet, Preserver}) ->
	%% handles {login,Group,Nick,Pid} signal from client
	%% and {clientExit, OnlineNum, Pid} signal from dmServant
	%% should also handle dmServant's unnormal EXIT signal
	receive
		{login, Group, Nick, From} ->
			%% client attempt to login, lookup ServantSet for the specified Group
			%% if exits, relay the signal to the particular dmServant
			%% if not, call dmServant:start( Group, Nick) to spwan a new dmServant
			%% 	and then relay the signal
			case ets:lookup( ServantSet, Group) of
				[] ->
					%% no one has logged in yet, start a new chat group
					DmServantPid = dmServant:start( Group, Nick, From, Preserver),
					ets:insert( ServantSet, {Group, DmServantPid, 1});
				[{Group, _DmServantPid, OnlineNum}] ->
					Group ! {login, Nick, From},
					ets:update_element(ServantSet, Group, {3, OnlineNum+1})
			end,
			%Group ! {login, Nick, From},
			loop({ServantSet, Preserver});

		{clientExit, OnlineNum, From} ->
			%% get Servant's registered name by calling process_info( Pid, registered_name)
			%% then update ServantSet
			{registered_name, ServantName} = process_info(From,registered_name),
			case OnlineNum of
				0 ->
					ServantName ! endServant,
					ets:delete(ServantSet,ServantName);
				_Any ->
					io:format([_Any]),
					ets:insert( ServantSet, {ServantName, From, OnlineNum})
			end,
			loop({ServantSet,Preserver});

		{'EXIT', Pid, normal} ->
			io:format("INFO : dmMaster : process ~p exited normally~n",[[pid_to_list(Pid)]]),
			loop({ServantSet,Preserver});

			
		{'EXIT', Pid, Why} ->
			%% A dmServant crashed
			%% or THE dmClientPreserver crashed
			case Pid of
				Preserver ->
					io:format("Preserver ~p crashed because of ~p~n",
						[pid_to_list(Pid),Why]),
					NewPreserver = dmClientPreserver:start(),
					loop({ServantSet, NewPreserver});
				_Any ->
					%% dmServant crashed, lookup the registered name,
					%% reboot it then send transOwner signal to dmPreserver
					[[ServantName, OnlineNum]] = ets:match( ServantSet, {'$1', Pid, '$2'}),
					io:format( "dmMaster : Process ~p of name ~p crashed because of ~p, ~p Users logged in it~n",[pid_to_list(Pid),ServantName,Why,OnlineNum]),

					NewServant = dmServant:reboot( ServantName),
					ets:insert( ServantSet, {ServantName, NewServant, OnlineNum}),
					dmClientPreserver ! {transOwner,ServantName,NewServant}
			end,
			loop({ServantSet,Preserver})


		%%{rebootDone,ServantName,From,OnlineNum} ->
			%% dmServant rebooted and received preserved clientSet, back into normal
			%%ets:insert( ServantSet, {ServantName,From,OnlineNum}),
			%%loop({ServantSet,Preserver});

	end.
