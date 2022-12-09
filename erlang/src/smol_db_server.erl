%%%-------------------------------------------------------------------
%% @doc belka example public API
%% @end
%%%-------------------------------------------------------------------

-module(smol_db_server).

-behaviour(application).

%% normal application API
-export([start/2, stop/1]).

%% function exported so it can be passed as a handler
%% to the Belka server
%% see the documentation for [Belka](https://github.com/gordonguthrie/belka/blob/main/src/belka.erl)
%% for an explanation of what happens under the hood
-export([router/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/smolsql/priv/keys/server.crt",
    KeyFile  = "/smolsql/priv/keys/server.key",
    _PID = belka:start(Port, CertFile, KeyFile, {smol_db_server, router}),
    smol_db_server_sup:start_link().

stop(_State) ->
    ok.

router(Route) ->
    {ok, Admins} = application:get_env(smol_db_server, admins),
    io:format("Admins are ~p~n", [Admins]),
    router(Route, Admins).

router(#{path := []} = Route, Admins) ->
    io:format("handler (1) got route ~p~n", [Route]),
    [
        "20 text/gemini\r\n",
        u("#  💻 Welcome to the Smol DB Server\r\n"),
        "This server implements a tiny subset of SQL and stores all data in human readable files.\r\n"
        "=> https://github.com/gordonguthrie/smolsql read more\r\n"
        "\r\n",
        u("## ⌨️ Configurations\r\n"),
        "This server is configured to only accept connections from one of these users:\r\n"
        ] ++ make_admins(Admins) ++
        [
        u("## 🎛 Setup\r\n"),
        "=> /dbs click here to see databases already created on this server\r\n",
        "To execute SQL\r\n"
        "* send a request to /db/<dbname>\r\n",
        "* on the 10 response post back the query parameters sql=\"sql statement\"\r\n"
        "\r\n"
        "Databases and tables are created on first-write\r\n",
        u("##❓SQL Supported\r\n"),
        "```\r\n",
        "INSERT INTO table VALUES (val1, val2, val3);\r\n",
        "INSERT INTO table (col1, col2, col3...) VALUES (val1, val2, val3...);\r\n",
        "SELECT * FROM table;\r\n",
        "SELECT col1, col2, col3... FROM table;\r\n",
        "SELECT [* | col1, col2, col3, col4...] FROM table WHERE (col1='alice' AND col2='bob'( OR (col3='charlie' AND col4 [=, <, >, =<. >=] 3);\r\n",
        "SELECT [* | col1, col2, col3] FROM table1 INNER JOIN table2 ON table1.col1 = table2.col2 WHERE...;\r\n",
        "DELETE FROM table WHERE ...;\r\n",
        "```\r\n"
    ];
router(#{path := ["db", DBName], querykvs := [{SQL, true}]} = Route, Admins) ->
    io:format("posting ~p to ~p~n", [SQL, DBName]),
    [
        "20 ok\r\n"
    ];
router(#{path := ["db", DBName]} = Route, Admins) ->
    io:format("posting sql to ~p~n", [DBName]),
    [
        "10 Post SQL\r\n"
    ];
router(#{path := ["dbs"]} = Route, Admins) ->
    case is_admin(Admins, Route) of
        true  ->
            io:format("list DBs here ~p~n", [Route]),
            {ok, DBRoot} = application:get_env(smol_db_server, dbroot),
            io:format("DBRoot is ~p~n", [DBRoot]),
            {ok, Dirs} = file:list_dir(DBRoot),
            Body = case Dirs of
                    [] -> ["No Databases created yet\r\n"];
                    _  -> ["### DBs are\r\n"] ++ ["* " ++ D ++ "\r\n" || D <- Dirs]
                end,
            [
                "20 text/gemini\r\n",
                u("## 🎛 DB Internals\r\n")
            ] ++ Body;
        false ->
            '60'()
        end;
router(A, _) ->
    io:format("A arrived ~p~n", [A]),
    exit(bajno).

make_admins(Admins) -> [lists:flatten([format(Name, Key) || #{key := Key, name := Name} <- Admins])].

format(Name, Key) ->
    [
        "### Name:\r\n",
        io_lib:format("~s", [Name]),
        "\r\n",
        "### Public Key:\r\n",
        io_lib:format("~p", [Key]),
        "\r\n"
    ].

is_admin([],                   _)                  -> false;
is_admin([#{key := K} | _T], #{id := #{key := K}}) -> true;
is_admin([_H | T],             Route)              -> is_admin(T, Route).

'51'() -> [u("51 Welcome to Area 51 👽\r\n")].
'60'() -> [u("60 Criminal Code Section 60 Violation 👮\r\n")].

 u(Text) -> unicode:characters_to_binary(Text, utf8).
