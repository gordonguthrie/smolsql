%%%-------------------------------------------------------------------
%% @doc belka example public API
%% @end
%%%-------------------------------------------------------------------

-module(smol_db_server).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/smolsql/priv/keys/server.crt",
    KeyFile  = "/smolsql/priv/keys/server.key",
    _PID = belka:start(Port, CertFile, KeyFile, {smol_db_routes, router}),
    smol_db_server_sup:start_link().

stop(_State) ->
    ok.
