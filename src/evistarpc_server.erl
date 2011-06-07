%% @author Wally Cash <wally.cash@gmail.com>
%% @copyright (C) 2010-2011, Wally Cash
%% @doc Implements a gen_server interface to the evistarpc rpc driver.
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

-module(evistarpc_server).

-behaviour(gen_server).

-export([start_link/0, context/1, login/2, rpc/1, rpc/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(lists, [nth/2, concat/1]).

-include("evistarpc.hrl").

host() -> get_env(host, "127.0.0.1").
port() -> get_env(port, 9201).



start_link() ->
	Host=host(),
	Port=port(),
    gen_server:start({local, ?MODULE}, ?MODULE, [Host, Port], []).


%%--------------------------------------------------------------------
%% @doc Set an application context
%% @end
%%--------------------------------------------------------------------

context(Context) when is_list(Context) -> gen_server:call(?MODULE, {context, Context}).

%%--------------------------------------------------------------------
%% @doc Login
%% @end
%%--------------------------------------------------------------------

login(Access, Verify) when is_list(Access), is_list(Verify)-> gen_server:call(?MODULE, {login, Access, Verify}).

%%--------------------------------------------------------------------
%% @doc Call an RPC
%% @end
%%--------------------------------------------------------------------

rpc(Request) when is_list(Request) -> gen_server:call(?MODULE, {rpc, Request}).

%%--------------------------------------------------------------------
%% @doc Call an RPC with arguments
%% @end
%%--------------------------------------------------------------------

rpc(Request, Args) when is_list(Request), is_list(Args) -> gen_server:call(?MODULE, {rpc, Request, Args}).

init([Host, Port]) ->
	case evistarpc_conn:start(Host, Port) of
	{ok,Pid} -> 
		error_logger:info_msg("~p: Connected to ~s:~w~n", [?MODULE,Host,Port]),
		evistarpc_conn:connect(Pid,["0.0.0.0","0", "evistarpc"]),
		process_flag(trap_exit, true),
		{ok,#state{driver_pid = Pid, host=Host, port=Port}};
	{error} ->
		error_logger:format("~p: Initialization error.~n", [?MODULE]), 
		{error, initialization_failure}
	end.

handle_call({context, Context}, _From, State) ->
	Y=evistarpc_conn:context(State#state.driver_pid, Context),
	{reply, Y, State};

handle_call({login, Access, Verify}, _From, State) ->
	{ok,A}=evistarpc_conn:login(State#state.driver_pid, Access, Verify),
	case A of 
	socket_closed ->
		error_logger:format("~s: No connection, login failed.~n", [?MODULE]),
		C="0",
		D="Login Failed",
		{error, socket_closed};
	_ ->
		B=string:tokens(A, "\r"),
		C =lists:nth(1, B),
		case C of
		"0" -> 
			case lists:nth(3, B) of
			"0" ->
				D=string:concat(nth(4, B),string:concat(nth(8,B), nth(9,B)));
			_ ->
				D=lists:nth(4, B)
			end;
		_ ->
			D=string:concat(nth(8, B), nth(9, B))
		end
	end,
   	{reply, {ok,{C,D}}, State};

handle_call({rpc, Request}, _From, State) ->
	Y=evistarpc_conn:rpc(State#state.driver_pid, Request),
   	{reply, Y, State};

handle_call({rpc, Request, Args}, _From, State) ->
	Y=evistarpc_conn:rpc(State#state.driver_pid, Request, Args),
   	{reply, Y, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	evistarpc_conn:disconnect(State#state.driver_pid),
	error_logger:info_msg("~p: stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_env(Key, Default) ->
    case application:get_env({{appid}}, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.



