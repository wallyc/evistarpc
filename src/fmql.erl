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


-module(fmql).  

-export([select/1, select/2, describe/1, describe/2]).

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------

select(File) ->
	["OP:SELECT^TYPE:" ++ File]. 

select(File, Args) ->
	["OP:SELECT^TYPE:" ++ File ++ Args]. 

describe(File) ->
	["OP:DESCRIBETYPE^TYPE:" ++ File]. 

describe(File, Args) ->
	["OP:DESCRIBETYPE^TYPE:" ++ File ++ Args]. 




















