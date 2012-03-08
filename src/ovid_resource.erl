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
%% GNU Affero General Public License for Monthre details.                      
%%                                                                         
%% You should have received a copy of the GNU Affero General Public License 
%% along with this program.  If not, see http://www.gnu.org/licenses/.


-module(ovid_resource).
  
-compile(export_all).
-export([encode_params/1, decode_ovid/1]).

-import(lists, [concat/1, nth/2, flatten/1]).
-import(mochinum, [digits/1]).
-import(string, [rstr/2, substr/3]).

-define(Char64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/").

%%--------------------------------------------------------------------
%% @doc Encode parameters for use with OVID.
%% Returns a list of key,value tuples of the form [{index, parameters}, ...].
%% @end
%%--------------------------------------------------------------------

encode_params(A) when is_list(A) ->
	encode_params(A, 1, []).

encode_params([H|T], I, Acc) ->
	case T of 
	[] -> 
		E = "f"; 
	_ -> 
		E = "" 
	end,
	encode_params(T, I + 1, [Acc|[{integer_to_list(I), encode_ovid(H) ++ E}]]);

encode_params([], _I, Acc) ->
	flatten(Acc).

%%--------------------------------------------------------------------
%% @doc Process OVID parameters.
%% @end
%%--------------------------------------------------------------------

encode_ovid(H) -> 
	encode_ovid(H, []).

encode_ovid([{K,V}|T], Acc) ->
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
		Pair = concat([int_to_B64(string:len(K)),K,int_to_B64(string:len(V2),2),V])
	end,
	encode_ovid(T, Acc ++ Pair);

encode_ovid([{C}|T], Acc) ->
	case {C} of
	{e} ->
		encode_ovid(T, Acc ++ "A");
	_ ->
		Compound=concat([int_to_B64(string:len(C)),C,"AA"]),
		encode_ovid(T, Acc ++ Compound)
	end;

encode_ovid([], Acc) ->
	Acc.

%%--------------------------------------------------------------------
%% @doc Process OVID Results.
%% @end
%%--------------------------------------------------------------------

decode_ovid(A) -> 
	decode_ovid(A, []).

decode_ovid(A, Acc) when size(A) > 0 ->
	<<Val:1/binary, Rest/binary>> = A,
	Pos = b64_to_int(binary_to_list(Val)),
	<<Val1:Pos/binary, Rest1/binary>> = Rest,
	case Rest1 of
	<<$A,$A, Rest2/binary>> ->
		Rest3=Rest2,
		Acc2 = Acc ++ [{binary_to_list(Val1)}];
	<<$A, Rest2/binary>> ->
		Rest3=Rest2,
		Acc2 = Acc ++ [{binary_to_list(Val1)}];
	_ ->
		case size(Rest1) of
		0 ->
			Acc2=Acc,
			Rest3 = <<>>,
			decode_ovid(A, Acc);
		_ ->
			{Val2, Rest3}=get_value(Rest1),
			[H|T]=lists:reverse(Acc), 
			Acc2= lists:reverse(T) ++ [{H, {binary_to_list(Val1), binary_to_list(Val2)}}]
		end
	end,
	decode_ovid(Rest3, Acc2);

 decode_ovid(Rest, Acc) when size(Rest) == 0 ->
  	Acc.

 get_value(V) ->
	<<Val:2/binary, Rest/binary>> = V,
	Pos=b64_to_int(binary_to_list(Val)),
	<<Val1:Pos/binary, Rest1/binary>> = Rest,
	case Rest1 of 
    <<$A, Rest2/binary>> ->
	 	Rest3=Rest2;
	_ ->
		Rest3=Rest1
	end,
	{Val1, Rest3}.

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

