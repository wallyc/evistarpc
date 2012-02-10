%% @author Wally Cash <wally.cash@gmail.com>
%% @copyright (C) 2010-2012, Wally Cash
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
-define(PING_INTERVAL, 60000).
-define(HEADER, "[XWB]1130").

%% Chunk types: Header=1, RPC=2, Security=3, Command=4, Data=5
%% Broker: Broker = [XWB], Version=1 , RPC Length=3 (i.e. ###)
%% RPC Types: Command=0, RPC=1 
%% Return Data: Return=0, No return=1
%%
%% Header=Broker + Version + Type + Length + Return

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
%% @doc Connect to the RPC broker.
%% @end
%%--------------------------------------------------------------------

connect(Pid, Str) ->
	rpc(Pid, "TCPConnect", Str).

%%--------------------------------------------------------------------
%% @doc Create an application context.
%% @end
%%--------------------------------------------------------------------

context(Pid, Context) ->
	X=encrypt(Context),
	rpc(Pid, "XWB CREATE CONTEXT",[X]).

%%--------------------------------------------------------------------
%% @doc Disconnect from the RPC broker.
%% @end
%%--------------------------------------------------------------------

disconnect(Pid) ->
	rpc(Pid, "#BYE#"),
	{bye}.

%%--------------------------------------------------------------------
%% @doc Login to the RPC broker.
%% @end
%%--------------------------------------------------------------------

login(Pid, Access, Verify) ->
	Upw=concat([Access, ";", Verify]),
 	X=encrypt(Upw),
	rpc(Pid, "XUS SIGNON SETUP"),
	rpc(Pid, "XUS AV CODE",[X]).

%%--------------------------------------------------------------------
%% @doc RPC call without parameters
%% @end
%%--------------------------------------------------------------------

rpc(Pid, Request) ->
	Header=make_header(Request),
	cmd(Pid, [Header, Request, "54f\x04"]).

%%--------------------------------------------------------------------
%% @doc RPC call with parameters
%% @end
%%--------------------------------------------------------------------

rpc(Pid, Request, Params) ->
	Header=make_header(Request),
	cmd(Pid, [Header, Request, "5", format_params(Params), "\x04"]).

%%--------------------------------------------------------------------
%% @doc Entry point for parameter formatting.
%% @end
%%--------------------------------------------------------------------

format_params(Params) ->
	case get_param_type(Params) of
	{literal} ->
		format_literal(Params, []);
	{kvlist} ->
		format_kvlist(Params);
	_ ->
		{error, invalid_parameters}
	end.

%%--------------------------------------------------------------------
%% @doc Format literal parameters.
%% @end
%%--------------------------------------------------------------------

format_literal([H|T], L) ->
	B=string:right(integer_to_list(string:len(H)), 3, $0),
	D=L ++ concat(["0", B, H, "f"]),
	format_literal(T, D);

format_literal([], L) ->
	L.

%%--------------------------------------------------------------------
%% @doc Format key/value parameter lists.
%% @end
%%--------------------------------------------------------------------

format_kvlist(Params) ->
	format_kvlist(Params, []).

format_kvlist([{K,V}|T], L) ->
	Key=string:right(integer_to_list(string:len(K)), 3, $0) ++ K,
	Value=string:right(integer_to_list(string:len(V)-1), 3, $0) ++ V,
	case T of
	[] ->
		Pair2 = concat([Key,Value]);
	_ ->
		Pair2 = concat([Key,Value,"t"])  
	end,
	format_kvlist(T, L ++ Pair2);

format_kvlist([], L) ->
	concat(["2", L]).

%%--------------------------------------------------------------------
%% @doc Construct the header.
%% @end
%%--------------------------------------------------------------------

make_header(Request) ->
	NameLen=[string:len(Request)],
 	if (Request =:= "TCPConnect") or (Request =:= "#BYE#")->
 		concat([?HEADER, "4", NameLen]); 
 	true ->
		concat([?HEADER, "2", "\x01", "1", NameLen])
 	end.

%%--------------------------------------------------------------------
%% @doc Get the parameter type. 
%% Currently scans for key/val lists and literals. Empty is hard coded 
%% as required.
%%
%% Types: Literal=0, Reference=1, List=2, Global=3, Empty=4
%% @end
%%--------------------------------------------------------------------

get_param_type(A) ->
	B=lists:flatten(A),
	get_param_type(B, []).

get_param_type([{_J,_K}|T], L) ->
	case L of 
	{literal} -> 
		get_param_type([], {invalid});
	_ ->
		get_param_type(T, {kvlist})
	end;

get_param_type([_H|T], L) ->
	case L of 
	{kvlist} -> 
		get_param_type([], {invalid});
	_ ->
		get_param_type(T, {literal})
	end;

get_param_type([], L) ->
	L.

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
%% @doc Strip the trailing \r \n 's.
%% @end
%%--------------------------------------------------------------------

strip_crlf(String) ->
    lists:reverse(drop_spaces(lists:reverse(String))).

drop_spaces([]) ->
    [];
drop_spaces(YS=[X|XS]) ->
    case is_space(X) of
        true ->
            drop_spaces(XS);
        false ->
            YS
    end.

is_space($\r) ->
    true;
is_space($\n) ->
    true;
is_space(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Broker listener/interface loop.
%% @end
%%--------------------------------------------------------------------

loop(Socket, Parent, Acc) ->
    receive
	{tcp, Socket, Bin} ->
		io:format("~n Bin:"),
		io:format(Bin),
		io:format("~n"),
		case Acc of 
		[] -> 
			case Bin of
			<<$\x00,$\x00, _/binary>> ->
				<<$\x00,$\x00, Rest/binary>> = Bin,
				Pos=size(Rest)-1;
			_ ->
				<<_, Rest/binary>> = Bin, 
				Pos=size(Rest)-2
			end;
		_ ->
			Rest = Bin, 
			Pos=size(Rest)
		end,
		<<Value:Pos/binary, _/binary>> = Rest,
		case re:run(Bin, "\x04") of
		nomatch -> 
			B=strip_crlf(binary_to_list(Value)),
			loop(Socket, Parent, Acc ++ B);
		{match, [{_, _}]} ->
			Pos2=size(Value)-1,	
			<<Value2:Pos2/binary, _/binary>> = Value,
			B=Acc ++ strip_crlf(binary_to_list(Value2)),
			Parent ! {self(), {ok, B}},
			loop(Socket, Parent, [])
		end;
	{tcp_error, Socket, Reason} ->
		Parent ! {self(), {error, socket_error}},
	    error_logger:format("Socket error:~p ~p~n", [Socket, Reason]);
	{tcp_closed, Socket} ->
		Parent ! {self(), {ok, socket_closed}},
	    error_logger:info_msg("Socket closed");
	{Parent, Request} -> 
		gen_tcp:send(Socket, list_to_binary(Request)),
		io:format("~n Request:"),
		io:format(Request),
	    loop(Socket, Parent, Acc)
    after ?PING_INTERVAL ->
		self() ! {Parent, "[XWB]11302\x011\x0bXWB IM HERE54f\x04"},
	    loop(Socket, Parent, Acc)
    end.

