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


-module(ovid_resource).
  
-compile(export_all).
-export([encode/1, to_struct/1]).

-import(lists, [concat/1, nth/2, flatten/1, reverse/1]).
-import(mochinum, [digits/1]).
-import(string, [rstr/2, substr/3]).

-define(Char64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/").

%%--------------------------------------------------------------------
%% @doc Encode parameters for use with OVID.
%% Returns a list of key,value tuples of the form [{index, parameters}, ...].
%% @end
%%--------------------------------------------------------------------

encode(A) ->
	encode(A, 1, []).

encode([H|T], I, Acc) ->
	case T of 
	[] -> 
		E = "f"; 
	_ -> 
		E = "" 
	end,
	encode(T, I + 1, [Acc|[{integer_to_list(I), encode_params(H) ++ E}]]);

encode([], _I, Acc) ->
	flatten(Acc).

%%--------------------------------------------------------------------
%% @doc Process OVID parameters.
%% @end
%%--------------------------------------------------------------------

encode_params(H) -> 
	encode_params(H, []).

encode_params([{K,V}|T], Acc) ->
	case V of
	[] ->
		Pair = concat([int_to_B64(string:len(K)),K]);
	_ ->
		case is_number(V) of
		true ->
			V2=digits(V);
		false ->
			V2=V
		end,
		Pair = concat([int_to_B64(string:len(K)),K,int_to_B64(string:len(V2),2),V2])
	end,
	encode_params(T, Acc ++ Pair);

encode_params([{C}|T], Acc) ->
	case {C} of
	{e} ->
		encode_params(T, Acc ++ "A");
	_ ->
		Compound=concat([int_to_B64(string:len(C)),C,"AA"]),
		encode_params(T, Acc ++ Compound)
	end;

encode_params([], Acc) ->
	Acc.

%%--------------------------------------------------------------------
%% @doc Parse OVID results to an Erlang data structure. 
%% @end
%%--------------------------------------------------------------------

to_struct(R) ->
	reverse(parse_tree(tokenize(R), [])).

parse_tree([H|T], Q) ->
	case H of
	{child, V} ->
		{K, R} = process_children(T, []),
		parse_tree(R, [{V, K}] ++ Q);
	{compound, V} ->
		parse_tree(T, [{V}] ++ Q);
	{close} ->
		parse_tree(T, Q);
	_ ->
		parse_tree(T, [H] ++ Q)
	end;

parse_tree([], Q) ->
	Q.

process_children(R = [H|T], Q) ->
	case is_end(H) of
	true ->
		case H of
		{close} ->
			Q2 = reverse(Q),
			{Q2, T};
		_ ->
			{Q, R}
		end;
	_ ->
		process_children(T, [H|Q])
	end.

is_end({close}) ->
    true;
is_end({child, _}) ->
    true;
is_end(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Tokenize an OVID result. 
%% @end
%%--------------------------------------------------------------------

tokenize(A) ->  
	{Val, Rest} = get_token(A, 1),
	tokenize_ovid(strip_crlf(Rest), [{compound, Val}], 2).

tokenize_ovid(R, Acc, N) when size(R) > 0 ->
	case R of
	<<$A,$A, Rest/binary>> ->
		case is_terminal(Rest) of
		true ->
			tokenize_ovid(Rest, [{close}, {close}] ++ Acc, N);
		false ->
			{Val, Rest1} = get_token(Rest, 1),
			tokenize_ovid(Rest1, [{compound, Val}] ++ Acc, 2)
		end;
	<<$A, Rest/binary>> ->
		tokenize_ovid(Rest, [{close}] ++ Acc, 1);
	_ ->
		{Val, Rest} = get_token(R, N),
		case N of 
		1 -> 
			tokenize_ovid(Rest, [{child, Val}] ++ Acc, 2);
		2 -> 
			[{_, J}|K]=Acc,
			tokenize_ovid(Rest, [{J, Val}|K], 1) 
		end
	end;

tokenize_ovid(R, Acc, _N) when size(R) == 0 ->
  	reverse(flatten(Acc)).

is_terminal(<<$A, _/binary>>) ->
	true;
is_terminal(<<>>) ->
	true;
is_terminal(_) ->
	false.

get_token(T, N) ->
	<<Val:N/binary, Rest/binary>> = T,
	Pos=b64_to_int(binary_to_list(Val)),
	<<Val1:Pos/binary, Rest1/binary>> = Rest,
	{Val1, Rest1}.

strip_crlf(A) ->
    recombine(re:split(A, [<<"\r\n">>], [trim]), <<>>).


recombine([H|T], Acc) ->
	recombine(T, <<Acc/binary, H/binary>>);

recombine([], Acc) ->
	Acc.


%%--------------------------------------------------------------------
%% @doc Convert an integer to Base 64 (1 or 2 digits). One digit is 
%% the default - little endian format for 2 digits.
%% @end
%%--------------------------------------------------------------------

int_to_B64(Num) when Num >= 0 andalso Num =< 63->
	int_to_B64(Num,1).

int_to_B64(Num,N) ->
	case N of 
	1 ->
		[nth(Num + 1,?Char64)];
	2 ->
		case Num < 64 of 
		true ->
			[nth(Num + 1,?Char64),nth(1,?Char64)]; 
		_ ->
			[nth((Num rem 64) + 1, ?Char64), nth((Num div 64) + 1, ?Char64)]
		end;
	_ ->
		{error, invalid_number_of_digits}	
	end.

%%--------------------------------------------------------------------
%% @doc Convert a Base 64 number to an integer (1 or 2 digits).
%% @end
%%--------------------------------------------------------------------

b64_to_int(N) ->
	case length(N) of  
	1 ->
		rstr(?Char64, N) - 1;
	2 ->
		A = rstr(?Char64, substr(N,1,1)) - 1,
		B = rstr(?Char64, substr(N,2,1)) - 1,
		(B * 64) + A;
	_ ->
		{error, invalid_number_of_digits}	
	end.

