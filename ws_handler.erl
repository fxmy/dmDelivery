-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	io:format("Req: ~p ~p~n",[Req,pid_to_list(self())]),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	%Self = self(),
	%%io:format("io:format~n"),
	%io:format( pid_to_list(Self)),
	%io:format("~n"),
	%io:format("Msg: ~p~n",[Msg]),
	%MMM = << <<"a">>/binary,Msg/binary>>,
	%Ma = binary_to_list(MMM),
	%io:format("Ma ~p~n",[Ma]),
	%Mb = list_to_atom(Ma),
	%io:format("Mb ~p~n",[Mb]),

	%[FirBin,SecBin,ThirBin] = binary:split(Msg, [<<",">>,<<" ">>], [global]),
	[FirBin,SecBin,ThirBin] = binary:split(Msg, [<<"\b">>], [global]),
	FirAtom = list_to_atom( binary_to_list( FirBin)),
	io:format("FirstAtom: ~p~n",[FirAtom]),

	case FirAtom of
		login ->
			%% ThirBin is nickname in binary form
			Group = list_to_atom( binary_to_list( SecBin)),
			StateNew = {Group,ThirBin},
			dmMaster ! {login,Group,ThirBin,self()},
			{reply, {text, << "logging in... ", Msg/binary >>}, Req, StateNew};
		chat ->
			{GroupDest,Nick} = State,
			io:format("dmServant : INFO : chat, ~p,~p,~p~n",[GroupDest,Nick,ThirBin]),
			GroupDest ! {chat,Nick,ThirBin,self()},
			%receive
			%	{chatUpdate,Nick,Text} ->
			%		Message = <<"chatUpdate,",Nick/binary,",",Text/binary>>,
			%		{reply, {text, Message}, Req, State}
			%end;

			{reply, {text, << "sengding... ", Msg/binary >>}, Req, State};
		Any ->
			io:format("Any: ~p~n",[[Any]]),
			{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State}
	end;

websocket_handle({chatUpdate, Nick, Text}, Req, State) ->
	io:format("WS_HANDLER : INFO : chatUpdate~n"),
	Message = <<"chatUpdate,",Nick/binary,",",Text/binary>>,
	{reply, {text, Message}, Req, State};


websocket_handle(_Data, Req, State) ->
		{ok, Req, State}.
	

	%% {reply, {text, << "That's what she said?", Msg/binary >>}, Req, State};
websocket_info(loginOK, Req, State) ->
	{reply, {text, <<"loginOK!!">>}, Req, State};

websocket_info({onLineUserUpdate, OnlineNum, OnlineUser}, Req, State) ->
	io:format("WS_HANDLER : INFO : onLineUserUpdate~n"),
	A = list_to_binary(integer_to_list(OnlineNum)),
	B = list_to_binary(OnlineUser),
	Message = <<"onlineUserUpdate,",A/binary,",",B/binary>>,
	{reply, {text, Message}, Req, State};


websocket_info({chatUpdate, Nick, Text}, Req, State) ->
	io:format("WS_HANDLER : INFO : chatUpdate~n"),
	Message = <<"chatUpdate,",Nick/binary,",",Text/binary>>,
	{reply, {text, Message}, Req, State};

	%{reply, {text, Message}, Req, State};

websocket_info({timeout, _Ref, Msg}, Req, State) ->

	%% infinite waits signal if dmServant here
	
	%receive
	%	loginOK ->
	%		{reply, {text, <<"loginOK">>}, Req, State};
	%	{onLineUserUpdate,OnlineNum,OnlineUser} ->
	%		A = list_to_binary(integer_to_list(OnlineNum)),
	%		B = list_to_binary(OnlineUser),
	%		Message = <<"onlineUserUpdate,",A,",",B>>,
	%		{reply, {text, Message}, Req, State};
	%	{chatUpdate,Nick,Text} ->
	%		Message = <<"chatUpdate,",Nick,",",Text>>,
	%		{reply, {text, Message}, Req, State};
	%	_ ->
			{reply, {text, <<"how's you doing?",Msg/binary>>}, Req, State};
	%after 0 ->
	%		{reply, {text, Msg}, Req, State}
	%end;


websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
	io:format("websocket terminiated ~p~n~p~n~p~n",[_Reason,_Req,State]),
	
	{Group,Nick} = State,
	Group ! {logout,Nick,self()},
	ok.
