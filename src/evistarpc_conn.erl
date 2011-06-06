%% @author Wally Cash <wally.cash@gmail.com>
%% @copyright (C) 2010-2011, Wally Cash
%% @doc
%% Provides basic facilities for interfacing with the VistA RPC broker.
%% @end
%%
%% This program is free software: you can redistribute it and/or modify     
%% it under the terms of the GNU Affero General Public License as           
%% published by the Free Software Foundation, either version 3 of the       
%% License, or (at your option) any later version.                          
%%                                                                         
%% This program is distributed in the hope that it will be useful,          
%% but WITHOUT ANY WARRANTY; without even the implied warranty of           
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            
%% GNU Affero General Public License for more details.                      
%%                                                                         
%% You should have received a copy of the GNU Affero General Public License 
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(evistarpc_conn). 

-compile(export_all).
 
-export([start/2, connect/2, context/2, disconnect/1, login/3, rpc/2, rpc/3]).

-define(RESPONSE_TIMEOUT, 5000).
-define(PING_INTERVAL, 14000).
-define(VERSION, "1.108").
-define(HEADER, "[XWB]11302\x05").

-import(lists, [concat/1,reverse/1]).
-import(evistarpc_cipher, [encrypt/1]).


%%--------------------------------------------------------------------
%% @doc Start the RPC interface listener.
%% @end
%%--------------------------------------------------------------------
start(Host, Port) when is_list(Host), is_integer(Port) -> 
	Parent=self(),
	Pid=spawn(fun() -> init(Parent, Host, Port) end),
    receive
	{Pid, Ret} ->
		case Ret of 
		ok ->
	    	{ok, Pid};
		error -> {error}
   		end
	end.

%%--------------------------------------------------------------------
%% @doc Init.
%% @end
%%--------------------------------------------------------------------

init(Parent, Host, Port) -> 
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
	{ok, Socket} -> 
		Parent ! {self(), ok},
		loop(Socket, Parent, []);
	{error, _} ->
	    Parent ! {self(), error}
	end.

%%--------------------------------------------------------------------
%% @doc Connect to an evistarpc RPC broker.
%% @end
%%--------------------------------------------------------------------
connect(Pid, Str) ->

	rpc(Pid, "TCPConnect", Str).

%%--------------------------------------------------------------------
%% @doc Create a Vista application context.
%% @end
%%--------------------------------------------------------------------

context(Pid, Context) ->
	X=encrypt(Context),
	rpc(Pid, "XWB CREATE CONTEXT",[X]).

%%--------------------------------------------------------------------
%% @doc Disconnect from a evistarpc RPC broker.
%% @end
%%--------------------------------------------------------------------

disconnect(Pid) ->
	rpc(Pid, "#BYE#"),
	{bye}.

%%--------------------------------------------------------------------
%% @doc Login to the evistarpc server.
%% @end
%%--------------------------------------------------------------------

login(Pid, Access, Verify) ->
	Upw=concat([Access, ";", Verify]),
 	X=encrypt(Upw),
	rpc(Pid, "XUS SIGNON SETUP"),
	rpc(Pid, "XUS AV CODE",[X]).

%%--------------------------------------------------------------------
%% @doc Call an RPC without arguments.
%% @end
%%--------------------------------------------------------------------

rpc(Pid, Request) ->
	Header=make_header(Request),
	R=concat([Header, Request, "54f\x04"]),
	cmd(Pid, {R}).

%%--------------------------------------------------------------------
%% @doc Call an RPC with arguments.
%% @end
%%--------------------------------------------------------------------

rpc(Pid, Request, Args) ->
	Header=make_header(Request),
	Fargs=format_args(Args),
	R=concat([Header, Request, "5", Fargs, "4f\x04"]),
	cmd(Pid, {R}).

%%--------------------------------------------------------------------
%% @doc Format the RPC Args.
%% @end
%%--------------------------------------------------------------------

format_args(Args) ->
	format_args(Args, []).

format_args([H|T], L) ->
	A=string:len(H),
	B=string:right(integer_to_list(A), 4, $0),
	C=concat([B, H, "f"]),
	D=L ++ C,
	format_args(T, D);

format_args([], L) ->
	L.

%%--------------------------------------------------------------------
%% @doc Construct the RPC Header.
%% @end
%%--------------------------------------------------------------------

make_header(Request) ->
	P=[string:len(Request)],
 	if (Request =:= "TCPConnect") or (Request =:= "#BYE#") ->
 		concat(["[XWB]10304", P]);
 	true ->
 		concat([?HEADER, ?VERSION, P])
 	end.

%%--------------------------------------------------------------------
%% @doc Send the RPC call to the listener process.
%% @end
%%--------------------------------------------------------------------

cmd(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
		Response
    after ?RESPONSE_TIMEOUT ->
	    {error, "timeout"}
	end.  

%%--------------------------------------------------------------------
%% @doc Broker listener/interface loop.
%% @end
%%--------------------------------------------------------------------

loop(Socket, Parent, B) ->
    receive
	{tcp, Socket, Bin} ->
		Buf=B ++ binary_to_list(Bin),
		case re:run(Buf, "\x04") of
		nomatch -> 
			loop(Socket, Parent, Buf);
		{match, _} ->
			B1=Buf -- "\x00,\x00,\x04",
			Parent ! {self(), {ok, B1}},
			loop(Socket, Parent, [])
		end;
	{tcp_error, Socket, Reason} ->
		Parent ! {self(), {error, socket_error}},
	    io:format("Socket error:~p ~p~n", [Socket, Reason]);
	{tcp_closed, Socket} ->
		Parent ! {self(), {ok, socket_closed}},
	    io:format("Socket closed ~n");
	{Parent, {Request}} -> 
		gen_tcp:send(Socket, Request),
	    loop(Socket, Parent, B)
    after ?PING_INTERVAL ->
		rpc(self(), "XWB IM HERE"),
		rpc(self(), "ORWU DT",["NOW"]),
	    loop(Socket, Parent, B)
    end.

