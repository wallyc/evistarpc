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


-module(ovid_fileman).  

-export([fm_update/3, fm_delete/2, fm_retrieve/3 ,fm_insert/2, fm_list/0 , fm_query_index/1, fm_query_list/2, fm_query_by_iens/3, 		fm_dictionary/1, fm_find/1, fm_refby/1, fm_screen_or/1, fm_screen_and/1, fm_screen_greater_than/1, fm_screen_equals/1, fm_screen_is_null/1, fm_screen_value/1, fm_screen_field/1]).

-import(lists, [concat/1, nth/2, flatten/1]).

%% Fields must be passed in as a list of strings as many vista fields
%% are of the form .## which do not form valid atoms.

%%--------------------------------------------------------------------
%% @doc Update a list of fields.
%% @end
%%--------------------------------------------------------------------

fm_update(File, Iens, Fields) when is_number(File), is_integer(Iens), is_list(Fields) ->
        [{"FILEMAN"}, {"UPDATE"}, {"FILE", File}, {"IENS", integer_to_list(Iens) ++ ","}] ++ format_fields(Fields) ++ [{e}, {e}].

%%--------------------------------------------------------------------
%% @doc Delete a record.
%% @end
%%--------------------------------------------------------------------

fm_delete(File, Iens) when is_number(File), is_integer(Iens) ->
        [{"FILEMAN"}, {"DELETE"}, {"FILE", File}, {"IENS", integer_to_list(Iens) ++ ","}, {e}, {e}].

%%--------------------------------------------------------------------
%% @doc Retieve a list of fields.
%% @end
%%--------------------------------------------------------------------

fm_retrieve(File, Iens, Fields) when is_number(File), is_integer(Iens), is_list(Fields) ->
	[{"FILEMAN"}, {"GETS"}, {"FILE", File}, {"IENS", integer_to_list(Iens) ++ ","}] ++ format_fields(Fields).

%%--------------------------------------------------------------------
%% @doc Insert
%% @end
%%--------------------------------------------------------------------

fm_insert(File, Fields) when is_number(File), is_list(Fields) ->
	[{"FILEMAN"}, {"INSERT"}, {"FILE", File}] ++ format_fields(Fields).

%%--------------------------------------------------------------------
%% @doc List of Files
%% @end
%%--------------------------------------------------------------------

fm_list() ->
	[{"FILEMAN"}, {"FILES"}, {e}, {e}].

%%--------------------------------------------------------------------
%% @doc Query Indexes.
%% @end
%%--------------------------------------------------------------------

fm_query_index(File) when is_number(File) ->
        [{"FILEMAN"}, {"INDEX"}, {"FILE", File}, {e}].

%%--------------------------------------------------------------------
%% @doc Query List.
%% @end
%%--------------------------------------------------------------------

fm_query_list(File, Fields) when is_number(File) or is_list(File), is_list(Fields) ->
        [{"FILEMAN"}, {"LIST"}, {"FILE", File}, {"PACK","1"}] ++ format_fields(Fields).
%% Does pack need to be passed in as a parameter? 
%% Is there ever an instance that we wouldn't want to pack the results?

%%--------------------------------------------------------------------
%% @doc Query By IENS.
%% @end
%%--------------------------------------------------------------------

fm_query_by_iens(File, Iens, Fields) when is_number(File), is_list(Iens), is_list(Fields) ->
        [{"FILEMAN"}, {"QIEN"}, {"FILE", File}] ++ format_iens(Iens) ++ format_fields(Fields).

%%--------------------------------------------------------------------
%% @doc Dictionary.
%% @end
%%--------------------------------------------------------------------

fm_dictionary(File) when is_number(File) or is_list(File)->
	[{"FILEMAN"}, {"DIC"}, {"FILE", File}, {e}].

%%--------------------------------------------------------------------
%% @doc Find.
%% @end
%%--------------------------------------------------------------------

fm_find({K, V}) ->
	[{"FIND"}, {"INDEX"}, {"NAME", K}, {"VALUE", V}, {e}].

%%--------------------------------------------------------------------
%% @doc Query Ref By.
%% @end
%%--------------------------------------------------------------------

fm_refby(File) when is_number(File) ->
	[{"FILEMAN"}, {"REFBY"}, {"FILE", File}, {e}].

%%--------------------------------------------------------------------
%% @doc Screen Or.
%% @end
%%--------------------------------------------------------------------

fm_screen_or({L, R}) ->
	[{"OR"}, {"LEFT", L}, {"RIGHT", R}, {e}].

%%--------------------------------------------------------------------
%% @doc Screen And.
%% @end
%%--------------------------------------------------------------------

fm_screen_and({L, R}) ->
	[{"AND"}, {"LEFT", L}, {"RIGHT", R}, {e}].

%%--------------------------------------------------------------------
%% @doc Screen Greater Than.
%% @end
%%--------------------------------------------------------------------

fm_screen_greater_than({L, R}) when is_number(L), is_number(L) ->
	[{"GT"}, {"LEFT", L}, {"RIGHT", R}, {e}].

%%--------------------------------------------------------------------
%% @doc Screen Equals.
%% @end
%%--------------------------------------------------------------------

fm_screen_equals({L, R}) ->
	[{"EQUALS"}, {"LEFT", L}, {"RIGHT", R}, {e}].

%%--------------------------------------------------------------------
%% @doc Screen Is Null.
%% @end
%%--------------------------------------------------------------------

fm_screen_is_null({F, B}) ->
	[{"ISNULL"}, {"FIELD", F}, {"BOOL", B}, {e}].

%%--------------------------------------------------------------------
%% @doc Screen Value.
%% @end
%%--------------------------------------------------------------------

fm_screen_value(V) ->
	[{"VALUE", V}].

%%--------------------------------------------------------------------
%% @doc Screen Field.
%% @end
%%--------------------------------------------------------------------

fm_screen_field(F) ->
	[{"FIELD", F}].

%%--------------------------------------------------------------------
%% @doc Internal.
%% @end
%%--------------------------------------------------------------------

format_fields(F) ->
	format_fields(F, []).

format_fields([{K,V}|T], Acc) ->
	B=Acc ++ [{"FIELD"}, {"NAME", K}, {"VALUE", V}, {e}],
	format_fields(T, B);

format_fields([{K,V,I}|T], Acc) ->
	case I of
	true -> 
		%% or should this be {"INTERNAL","\"(I)\""}? Is it call sensitive?
		B=Acc ++ [{"FIELD"}, {"NAME", K}, {"VALUE", V}, {"INTERNAL","1"},{e}];
	false ->
	    B=Acc ++ [{"FIELD"}, {"NAME", K}, {"VALUE", V}, {e}];
	_ ->
		B={error}
	end,
	format_fields(T, B);

format_fields([H|T], Acc) ->
	B=Acc ++ [{"FIELD"}, {"NAME", H}, {e}],
	format_fields(T, B);

format_fields([], Acc) ->
	Acc.

format_iens(I) ->
	format_iens(I, []).

format_iens([H|T], Acc) ->
	B=Acc ++ [{"IENS", H ++ ","}],
	format_iens(T, B);

format_iens([], Acc) ->
	Acc.




















