%% Feel free to use, reuse and abuse the code in this file.
%%
%% %% @doc Stream handler for clock synchronizing.
-module(stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 9000).

init(_Transport, Req, _Opts, Active) ->
	case Active of
		once ->
			{http_req,_Port,ranch_tcp,keepalive,_Pid,_Method,_,
				{{_IPA,_IPB,_IPC,_IPD},_FromPort},
				_,_,_,_HandlerPath,_,
				_,_,_,_,
				AttrList,
				_ConnectionInfoList,
				_,_,_,_,_,_,_,_,_,_} = Req,
			[Cookie |_] = [ Cookie || {<<"cookie">>, Cookie} <- AttrList],
			[URL |_] = [ URL || {<<"referer">>, URL} <- AttrList],

			Nick = Cookie,
			UrlTokens = binary:split( URL, <<"/">>, [global]),
			[_|[_|[_|ChannelAndAfter]]] = UrlTokens,
			[ChannelBinary|_Rest] = ChannelAndAfter,
			Channel = list_to_atom( binary_to_list( ChannelBinary)),
			StateNew = {Channel,Nick},
			io:format("once~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~n"),
			dmMaster ! {login, Channel, Nick, self()};

		true ->
			{http_req,_Port,ranch_tcp,keepalive,Pid,<<"GET">>,
				_,
				{{_IPA,_IPB,_IPC,_IPD},_FromPort},
				_,_,_,ReqPath,_,_,_,_,_,
				AttrList,
				_UpgradeInfoList,_,
				[{websocket_version,_}],_,_,_,_,_,_,_,_} = Req,
			
			Cookie = [ Cookie || {<<"cookie">>,Cookie} <- AttrList],
			[Nick|_] = Cookie,
			PathTokens = binary:split(ReqPath, <<"/">>, [global]),
			[_|[_|[ChannelBinary|_Rest]]] = PathTokens,
			Channel = list_to_atom( binary_to_list( ChannelBinary)),
			dmMaster ! {login, Channel, Nick, self()},
			StateNew = {Channel, Nick};

		_Other ->
			StateNew = undefinedOther
	end,
	
	_ = erlang:send_after(?PERIOD, self(), refresh),
	{ok, Req, StateNew}.

stream(<<"ping">>, Req, State) ->
	io:format("ping received ~p~n", [pid_to_list(self())]),
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
	io:format("stream received ~s ~p~n", [Data, pid_to_list(self())]),

	[FirBin,SecBin,ThirBin,FourBin] = binary:split(Data, [<<"\b">>], [global]),
	FirAtom = list_to_atom( binary_to_list( FirBin)),
	case FirAtom of
		chat ->
			GroupDest = list_to_atom( binary_to_list( ThirBin)),
			Nick = SecBin,
			%dmMaster ! {login,GroupDest,Nick,self()},
			io:format("STREAM : INFO : chat, ~p,~p,~p,~p~n",[GroupDest,Nick,ThirBin,pid_to_list(self())]),
			io:format("State: ~p~n",[State]),
			
			%erlang:start_timer(1000, self(), {GroupDest,chat,Nick, FourBin}),
			GroupDest ! {chat,Nick,FourBin,self()},
			StateNew = State;
		Any ->
			io:format("Any: ~p~n",[[Any]]),
			StateNew = State
	end,

	{ok, Req, StateNew}.
info({timeout, _TimerRef, {GroupDest,chat,Nick,FourBin}}, Req, State) ->
	io:format("timeout!! ~p~n", [pid_to_list(self())]),
	GroupDest ! {chat,Nick, FourBin, self()},
	{ok, Req, State};
info(refresh, Req, State) ->
	%% _ = erlang:send_after(?PERIOD, self(), refresh),
	DateTime = cowboy_clock:rfc1123(),
	io:format("clock refresh timeout: ~s ~p~n", [DateTime, pid_to_list(self())]),
	{reply, DateTime, Req, State};
info(loginOK, Req, State) ->
	io:format("info loginOK~n"),
	{ok, Req, State};
info({chatUpdate, Nick, Text}, Req, State) ->
	Message = <<"chatUpdate,",Nick/binary,",",Text/binary>>,
	{reply, Message, Req, State};
	%io:format("info received : ~p~n",[Message]),
	%{ok, Req, State};
info(Info, Req, State) ->
	io:format("INFO received ~p ~p~n", [Info, pid_to_list(self())]),
	{ok, Req, State}.

terminate(Req, State) ->
	io:format("bullet terminate ~p~n", [pid_to_list(self())]),
	%io:format("~p~n", [Req]),
	io:format("~p~n", [State]),
	case Req of
		{http_req,_Port,ranch_tcp,keepalive,Pid,<<"GET">>,
			_,
			{{_IPA,_IPB,_IPC,_IPD},_FromPort},
			_,_,_,_,_,
			_,_,_,_,_,_,_,_,_,
			_,_,_,_,_,_,_} ->

			{Channel,Nick} = State,
			Channel ! {logout,Nick,Pid};
		_Other ->
			void
	end,
	ok.
