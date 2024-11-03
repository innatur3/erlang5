-module(key_value_benchmark).
-export([benchmark/0]).

%% Таблиця
benchmark() ->
    io:format("~nComparison of Key-Value Storage Mechanisms~n"),
    io:format("Mechanism\t\tInsert (ms)\tUpdate (ms)\tDelete (ms)\tRead (ms)~n"),
    io:format("---------------------------------------------------------------~n"),
    
    %% map
    {InsertMap, UpdateMap, DeleteMap, ReadMap} = measure_map(),
    io:format("map\t\t\t~p\t\t~p\t\t~p\t\t~p~n", [InsertMap, UpdateMap, DeleteMap, ReadMap]),
    
    %% proplist
    {InsertProplist, UpdateProplist, DeleteProplist, ReadProplist} = measure_proplist(),
    io:format("proplist\t\t~p\t\t~p\t\t~p\t\t~p~n", [InsertProplist, UpdateProplist, DeleteProplist, ReadProplist]),
    
    %% Process Dictionary
    {InsertPD, UpdatePD, DeletePD, ReadPD} = measure_process_dict(),
    io:format("Process Dict\t~p\t\t~p\t\t~p\t\t~p~n", [InsertPD, UpdatePD, DeletePD, ReadPD]),
    
    %% dict
    {InsertDict, UpdateDict, DeleteDict, ReadDict} = measure_dict(),
    io:format("dict\t\t\t~p\t\t~p\t\t~p\t\t~p~n", [InsertDict, UpdateDict, DeleteDict, ReadDict]),
    
    %% record
    {InsertRecord, UpdateRecord, DeleteRecord, ReadRecord} = measure_record(),
    io:format("record\t\t\t~p\t\t~p\t\t~p\t\t~p~n", [InsertRecord, UpdateRecord, DeleteRecord, ReadRecord]),
    
    %% ETS
    {InsertETS, UpdateETS, DeleteETS, ReadETS} = measure_ets(),
    io:format("ETS\t\t\t~p\t\t~p\t\t~p\t\t~p~n", [InsertETS, UpdateETS, DeleteETS, ReadETS]),
    
    %% DETS
    {InsertDETS, UpdateDETS, DeleteDETS, ReadDETS} = measure_dets(),
    io:format("DETS\t\t\t~p\t\t~p\t\t~p\t\t~p~n", [InsertDETS, UpdateDETS, DeleteDETS, ReadDETS]).

%% Вимірювання для map
measure_map() ->
    Map = #{},
    {Insert, _} = timer:tc(fun() -> maps:put(test, 123, Map) end),
    Map2 = maps:put(test, 123, Map),
    {Update, _} = timer:tc(fun() -> maps:put(test, 456, Map2) end),
    {Delete, _} = timer:tc(fun() -> maps:remove(test, Map2) end),
    {Read, _} = timer:tc(fun() -> maps:get(test, Map2) end),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.

%% Вимірювання для proplist
measure_proplist() ->
    Proplist = [],
    {Insert, _} = timer:tc(fun() -> [{test, 123} | Proplist] end),
    Proplist2 = [{test, 123} | Proplist],
    {Update, _} = timer:tc(fun() -> lists:keyreplace(test, 1, Proplist2, {test, 456}) end),
    {Delete, _} = timer:tc(fun() -> lists:keydelete(test, 1, Proplist2) end),
    {Read, _} = timer:tc(fun() -> lists:keyfind(test, 1, Proplist2) end),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.

%% Вимірювання для Process Dictionary
measure_process_dict() ->
    erlang:put(test, 123),
    {Insert, _} = timer:tc(fun() -> erlang:put(test, 123) end),
    {Update, _} = timer:tc(fun() -> erlang:put(test, 456) end),
    {Delete, _} = timer:tc(fun() -> erlang:erase(test) end),
    {Read, _} = timer:tc(fun() -> erlang:get(test) end),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.

%% Вимірювання для dict
measure_dict() ->
    Dict = dict:new(),
    {Insert, _} = timer:tc(fun() -> dict:store(test, 123, Dict) end),
    Dict2 = dict:store(test, 123, Dict),
    {Update, _} = timer:tc(fun() -> dict:store(test, 456, Dict2) end),
    {Delete, _} = timer:tc(fun() -> dict:erase(test, Dict2) end),
    {Read, _} = timer:tc(fun() -> dict:fetch(test, Dict2) end),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.

%% Вимірювання для record
-record(test, {key = undefined, value = undefined}).
measure_record() ->
    Record = #test{},
    {Insert, _} = timer:tc(fun() -> Record#test{key = test, value = 123} end),
    Record2 = Record#test{key = test, value = 123},
    {Update, _} = timer:tc(fun() -> Record2#test{value = 456} end),
    {Delete, _} = timer:tc(fun() -> Record#test{key = undefined, value = undefined} end),
    {Read, _} = timer:tc(fun() -> Record2#test.value end),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.

%% Вимірювання для ETS
measure_ets() ->
    Table = ets:new(test_table, [named_table, public]),
    {Insert, _} = timer:tc(fun() -> ets:insert(Table, {test, 123}) end),
    {Update, _} = timer:tc(fun() -> ets:insert(Table, {test, 456}) end),
    {Delete, _} = timer:tc(fun() -> ets:delete(Table, test) end),
    {Read, _} = timer:tc(fun() -> ets:lookup(Table, test) end),
    ets:delete(Table),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.

%% Вимірювання для DETS
measure_dets() ->
    {ok, Table} = dets:open_file(test_table, [{type, set}]),
    {Insert, _} = timer:tc(fun() -> dets:insert(Table, {test, 123}) end),
    {Update, _} = timer:tc(fun() -> dets:insert(Table, {test, 456}) end),
    {Delete, _} = timer:tc(fun() -> dets:delete(Table, test) end),
    {Read, _} = timer:tc(fun() -> dets:lookup(Table, test) end),
    dets:close(Table),
    {Insert div 1000, Update div 1000, Delete div 1000, Read div 1000}.
