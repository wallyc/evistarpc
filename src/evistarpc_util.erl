%% @author Wally Cash <wally.cash@gmail.com>
%% @copyright (C) 2010-2012, Wally Cash
%% @doc A collection of utilities to manipulate VistA data structures. 
%% @end
%%
%% This program is free software: you can redistribute it and/or Modify     
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
%% along with this program.  If not, see http://www.gnu.org/licenses/.


-module(evistarpc_util).  

-export([piece/2, piece/3, from_fm_date/1, to_fm_date/1, to_fm_datetime/1, from_fm_datetime/1, to_record/2, process_response/1,
		strip_crlf/1]).

-include("evistarpc.hrl").

-import(lists, [concat/1, nth/2, flatten/1]).

%%--------------------------------------------------------------------
%% @doc Parses a \r\n delimited string to a key value record. Intended 
%% to populate Nitrogen combo/list boxes via the record syntax.
%% @end
%%--------------------------------------------------------------------

to_record(Rec, Str) when is_list(Str) ->	
	L=string:tokens(Str, "\r\n"),
	[Rec#listdata{key=piece(Y, 2),value=piece(Y, 1)} || Y <- L].

%%--------------------------------------------------------------------
%% @doc Parses a '\r\n' delimited binary to an erlang stucture which 
%% can be further processed by mochijson2 to JSON.
%% @end
%%--------------------------------------------------------------------

process_response(H) -> 
	process_response(binary:split(H,[<<"\r\n">>],[global, trim]), []).

process_response([H|T], Acc) ->
	process_response(T, Acc ++ get_pair(H));

process_response([], Acc) ->
	{struct, Acc}.

get_pair(H) ->
	[J,K] = binary:split(H, <<"^">>),
	[{J,K}].

%%--------------------------------------------------------------------
%% @doc Extracts an element from a '^' delimited list specified
%% by its position.
%% @end
%%--------------------------------------------------------------------

piece(Str, Pos) when is_list(Str), is_integer(Pos) ->
    piece1(Str,Pos,1,"^",[]).

%%--------------------------------------------------------------------
%% @doc Extracts an element from a delimited list specified
%% by its position and delimiter.
%% @end
%%--------------------------------------------------------------------

piece(Str, Pos, Sep) when is_list(Str), is_integer(Pos), is_list(Sep) ->
    piece1(Str, Pos, 1, Sep, []).

piece1([H|T], Pos, J, Sep, Acc) ->
    case lists:member(H, Sep) of
	true -> 
		case J=:=Pos of
		true ->		
			piece1([], Pos, J+1, Sep, Acc);
		false ->
			piece1(T, Pos, J+1, Sep, [])
		end;
	false -> 
		piece1(T, Pos, J, Sep, [H|Acc])
    end;

piece1([], _I, _J, _Sep, Acc)-> lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @doc Converts a fileman date string to a date tuple.
%% @end
%%--------------------------------------------------------------------

from_fm_date(Date) when is_list(Date) ->
	{Yr,_}=string:to_integer(string:sub_string(Date,1,3)),
	Year=Yr+1700,
	Month=list_to_integer(string:sub_string(Date,4,5)),
	Day=list_to_integer(string:sub_string(Date,6,7)),
	{Year,Month,Day}.

%%--------------------------------------------------------------------
%% @doc Converts an Erlang date tuple to fileman format.
%% @end
%%--------------------------------------------------------------------

to_fm_date(Date) when is_tuple(Date) ->
    {{Year,Month,Day}} = Date,
	Year2=Year-1700,
    D=io_lib:format("~3.10.0B~2.10.0B~2.10.0B",[Year2, Month, Day]),
	lists:flatten(D).

%%--------------------------------------------------------------------
%% @doc Converts an Erlang datetime tuple to fileman format.
%% @end
%%--------------------------------------------------------------------

to_fm_datetime(DateTime) when is_tuple(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
	Year2=Year-1700,
    D=io_lib:format("~3.10.0B~2.10.0B~2.10.0B.~2.10.0B~2.10.0B~2.10.0B",[Year2, Month, Day, Hour, Min, Sec]),
	lists:flatten(D).

%%--------------------------------------------------------------------
%% @doc Converts a fileman datetime string to a datetime tuple.
%% @end
%%------------------------------------------------------------------01---

from_fm_datetime(DateTime) ->
	{Yr,_}=string:to_integer(string:sub_string(DateTime,1,3)),
	Year=Yr+1700,
	Month=list_to_integer(string:sub_string(DateTime,4,5)),
	Day=list_to_integer(string:sub_string(DateTime,6,7)),
	Hour=list_to_integer(string:sub_string(DateTime,9,10)),
	Min=list_to_integer(string:sub_string(DateTime,11,12)),
	Sec=list_to_integer(string:sub_string(DateTime,13,14)),
	{{Year,Month,Day},{Hour,Min,Sec}}.

%%--------------------------------------------------------------------
%% @doc Strip the trailing \r \n's.
%% @end
%%--------------------------------------------------------------------

strip_crlf(A) ->
    drop_spaces(A).

drop_spaces(<<>>) ->
    <<>>;
drop_spaces(A) ->
	B=binary:last(A),
	case is_space(B) of
		true ->
			C=size(A)-1,
			<<Val:C/binary, _/binary>> =A,
		    drop_spaces(Val);
		false ->
		    A
	end.

is_space(13) ->
    true;
is_space(10) ->
    true;
is_space(32) ->
    true;
is_space(_) ->
    false.

