-module(dmClientPreserver).
-export([start/0,stop/0]).

%% preservers a list of crashed dmServants' clients
%% [sets] where set = tid()
start() ->
	PreserverPid = spawn_link(dmClientPreserver, loop, []),
	register(dmClientPreserver, PreserverPid),

	PreserverPid ! init,

	PreserverPid.

%% real work handled here.
%% loop waits for a {'ETS-TRANSFER', tid(), FromPid, HeirData} to preserver a crashed dmServant
%%                a {transOwner, Group, ServantPid} to return the SET
loop( Preservered) ->
	receive
		init ->
			%% Master send init signal, set up a empty SET of {channel, tid()}
			PreservedTable = ets:new(preservedTable, [set]),
			loop(PreservedTable);

		{'ETS-TRANSFER', TableId, FromPid, HeirData} ->
			%% dmServat crashed
			%% HeirData will be the group name
			ets:insert(PreservedTable, {HeirData, TableId}),
			loop(PreservedTable);

		{transOwner, Group, ServantPid} ->
			%% dmMaster sends signal to return the table back
			[{_,TabID}] = ets:lookup(PreservedTable,Group),
			ets:give_away(TabID, ServantPid, []),
			ets:delete(PreservedTable, Group),
			loop(PreservedTable)
	end.

stop() ->
	true.
