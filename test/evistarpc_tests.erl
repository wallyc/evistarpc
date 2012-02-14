-module(evistarpc_tests).

%%-include_lib("evistarpc.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(evistarpc_conn, [get_param_type/1, format_params/1, format_kvlist/1, format_literal/1]). 
-import(evistarpc_util, [int_to_B64/1, int_to_B64/2, encode_ovid_params/1, piece/2, piece/3, to_fm_datetime/1,
						from_fm_datetime/1, to_fm_date/1, from_fm_date/1, to_json/1, to_json/2]). 

get_parameter_type_test() ->
    ?assertEqual({literal}, get_param_type(["TEST"])),
    ?assertEqual({kvlist}, get_param_type([{"KEY","VALUE"}])),
    ?assertEqual({kvlist}, get_param_type([{"KEY","VALUE"},{"KEY","VALUE"}])),
    ?assertEqual({invalid}, get_param_type([{"KEY"},{"VALUE"}])),
    ?assertEqual({invalid}, get_param_type(["KEY",{"KEY","VALUE"}])),
    ?assertEqual({invalid}, get_param_type([{"KEY","VALUE"},"KEY"])),
    ?assertEqual({invalid}, get_param_type(["KEY",{"VALUE"}])),
    ?assertEqual({invalid}, get_param_type([{"KEY"},"VALUE"])).

format_parameters_test() ->
	?assertEqual([48,48,49,54,"OPEN VISTA ROCKS",102], format_params(["OPEN VISTA ROCKS"])),
	?assertEqual("2003one002twot005three003four", format_kvlist([{"one","two"},{"three","four"}])),
	?assertEqual("0010abcde fghif", format_literal("abcde fghi")).

utils_test() ->	
	?assertEqual("A", int_to_B64(0)),
	?assertEqual("IA", int_to_B64(8,2)),
	?assertEqual("BB", int_to_B64(65,2)),
	P=[[{"FILEMAN"},{"LIST"},{"FILE","4"},{"PACK","1"},{"FIELD"},{"NAME",".01"},{e}]],
	R=[{"1","HFILEMANAAELISTAAEFILEBA4EPACKBA1FFIELDAAENAMEDA.01AAAf"}],
	?assertEqual(R, encode_ovid_params(P)),
	?assertEqual({2012,02,12}, from_fm_date("3120212")),	
	?assertEqual("3120212", to_fm_date({{2012,02,12}})),
	?assertEqual("3120212.010203", to_fm_datetime({{2012,02,12},{01,02,03}})),
	?assertEqual({{2012,02,12},{01,02,03}}, from_fm_datetime("3120212,010203")).














