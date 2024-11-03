-module(cache).
-export([init/0, put/2, put/3, get/1, remove_expired/0]).

-record(entry, {value, expiration}).

init() ->
    ets:new(cache_table, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]).

put(Key, Value) ->
    put(Key, Value, infinity).

put(Key, Value, Timeout) when is_integer(Timeout) ->
    Expiration = case Timeout of
        infinity -> infinity;
        _ -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Timeout
    end,
    ets:insert(cache_table, {Key, #entry{value = Value, expiration = Expiration}}).

get(Key) ->
    case ets:lookup(cache_table, Key) of
        [{Key, #entry{value = Value, expiration = ExpiryTime}}] ->
            CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            if 
                ExpiryTime == infinity; CurrentTime =< ExpiryTime -> Value;
                true -> ets:delete(cache_table, Key), undefined
            end;
        [] -> undefined
    end.

remove_expired() ->
    CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    [ets:delete(cache_table, Key) || {Key, #entry{expiration = ExpiryTime}} <- ets:tab2list(cache_table), ExpiryTime =/= infinity, ExpiryTime =< CurrentTime],
    ok.