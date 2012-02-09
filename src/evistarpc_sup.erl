%% @author Wally Cash <wally.cash@gmail.com>
%% @copyright (C) 2010-2012, Wally Cash
%% @doc Supervising process.
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

-module(evistarpc_sup).

-behaviour(supervisor).

-export([start/0, start_link/1]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start() ->
    spawn(fun() ->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	  end).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    EVistaRpc = ?CHILD(evistarpc_server, worker),
    {ok, { {one_for_one, 5, 10}, [EVistaRpc]} }.

