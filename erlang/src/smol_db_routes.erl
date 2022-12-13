-module(smol_db_routes).

%% function exported so it can be passed as a handler
%% to the Belka server
%% see the documentation for [Belka](https://github.com/gordonguthrie/belka/blob/main/src/belka.erl)
%% for an explanation of what happens under the hood
-export([router/1]).

%% exported to be used as a callback in expanding templates
-export([
			format_admins_fn/1,
			format_dbs_fn/1,
			format_tables_fn/1
		]).

router(Route) ->
    {ok, Admins} = application:get_env(smol_db_server, admins),
    IsAdmin = is_admin(Admins, Route),
    router(Route, Admins, IsAdmin).

%% the route for the home page
router(#{path := []}, Admins, _) ->
	{ok, DBs} = smol_sql_engine:list_dbs(),
    Params = [{admins, Admins}, {dbs, DBs}],
    belka_templates:render(smol_db_routes, "frontpage", Params);

%% create new tables in a database
router(#{path := ["dbs", "create"], querykvs := []}, _, true) ->
	["10 what is the name of the new database?\r\n"];
router(#{path := ["dbs", "create"], querykvs := [{DBName, true}]}, _, true) ->
	{ok, DB} = smol_sql_engine:create_db(DBName),
	["30 /db/" ++ DB ++ "\r\n"];
router(#{path := ["dbs", DBName, "create"], querykvs := [{TableName, true}]}, _, true) ->
	{ok, DB} = smol_sql_engine:create_db(DBName),
	belka_templates:render("create_table", [{db, DB}, {table, TableName}]);
router(#{path := ["dbs", DBName, "create" ], querykvs := []}, _, true) ->
	["10 what is the name of the new table in " ++ DBName ++ "?\r\n"];
router(#{path := ["dbs", DBName, "create", TableName], querykvs := []}, _, true) ->
    [
        "10 Post Table Definition of the table:" ++ TableName ++ " in db:" ++ DBName ++ "\r\n"
    ];
router(#{path := ["dbs", DBName, "create", TableName], querykvs := [{TableDef, true}]}, _, true) ->
    io:format("posting table def ~p to create ~p on ~p~n", [TableDef, TableName, DBName]),
	case smol_sql_engine:create_table(TableDef, DBName, TableName) of
		{ok, Table} -> ["20 text/gemini\r\ntable " ++ Table ++ " created\r\n"];
		{error, _E} -> ["20 text/gemini\r\ntable " ++ TableName ++ " NOT created\r\n"]
	end;

%% show all the tables that exist
router(#{path := ["db", DBName], querykvs := []}, _, true) ->
	case smol_sql_engine:list_tables(DBName) of
    	{ok, []}     -> belka_templates:render("tables_none", [{db, DBName}]);
    	{ok, Tables} -> Params = [{db, DBName}, {tables, Tables}],
    					io:format("in router Params is ~p~n", [Params]),
    					belka_templates:render(smol_db_routes, "tables_exist", Params)
    end;

%% handle not founds and log in attempts that fail
router(Route, _, true)  -> io:format("in route 51 for ~p~n", [Route]),
                           '51'();
router(_, _, false)     -> '60'().


format_admins_fn(Params) ->
	Admins = proplists:get_value(admins, Params),
	Format = "### Name:~n~s~n### Public Key: ~n~p~n",
	[io_lib:format(Format, [N, K]) || #{name := N, key := K} <- Admins].

format_dbs_fn(Params) ->
	DBs = proplists:get_value(dbs, Params),
	case DBs of
		[] -> io_lib:format("There are no databases yet~n", []);
		_  -> [io_lib:format("=> /db/~s ~s has ~p tables~n", [D, D, length(Ts)]) || {D, Ts} <- DBs]
	end.

format_tables_fn(Params) ->
	io:format("in format_tables_fn Params is ~p~n", [Params]),
	DB     = proplists:get_value(db, Params),
	Tables = proplists:get_value(tables, Params),
	[io_lib:format("=> /db/~s/~s ~s~n", [DB, T, T]) || {T, _} <- Tables].

is_admin([],                   _)                  -> false;
is_admin([#{key := K} | _T], #{id := #{key := K}}) -> true;
is_admin([_H | T],             Route)              -> is_admin(T, Route).

'51'() -> [u("51 Welcome to Area 51 👽\r\n")].
'60'() -> [u("60 Criminal Code Section 60 Violation 👮\r\n")].

u(Text) -> unicode:characters_to_binary(Text, utf8).

