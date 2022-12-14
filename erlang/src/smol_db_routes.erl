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
			format_tables_fn/1,
			format_table_details_fn/1,
			table_creation_errors_format_fn/1
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
	CleanTable = smol_sql_engine:clean(TableName),
	belka_templates:render("create_table", [{db, DB}, {table, CleanTable}]);
router(#{path := ["dbs", DBName, "create" ], querykvs := []}, _, true) ->
	["10 what is the name of the new table in " ++ DBName ++ "?\r\n"];
router(#{path := ["dbs", DBName, "create", TableName], querykvs := []}, _, true) ->
    [
        "10 Post Table Definition of the table:" ++ TableName ++ " in db:" ++ DBName ++ "\r\n"
    ];
router(#{path := ["dbs", DBName, "create", TableName], querykvs := [{TableDef, true}]}, _, true) ->
	case smol_sql_engine:create_table(TableDef, DBName, TableName) of
		{ok, {DB, Table}} ->
			["30 /db/" ++ DB ++ "/" ++ Table ++ "\r\n"];
		{error, Es} ->
			Params = [{db,     DBName},
                      {table,  TableName},
                      {errors, Es}],
			belka_templates:render(smol_db_routes, "cant_create_table", Params)
	end;

%% show all the tables that exist
router(#{path := ["db", DBName], querykvs := []}, _, true) ->
	case smol_sql_engine:list_tables(DBName) of
    	{ok, []}     -> belka_templates:render("tables_none", [{db, DBName}]);
    	{ok, Tables} -> Params = [{db, DBName}, {tables, Tables}],
    					belka_templates:render(smol_db_routes, "tables_exist", Params)
    end;

%% show details of a table
router(#{path := ["db", DBName, TableName], querykvs := []}, _, true) ->
	case smol_sql_engine:get_table_details(DBName, TableName) of
    	{ok, Desc} ->
    		belka_templates:render(smol_db_routes, "table_details", Desc);
    	{error, Error} ->
    		io:format("Error is ~p~n", [Error]),
    		'51'()
    end;


%% handle not founds and log in attempts that fail
router(Route, _, true)  -> io:format("in route 51 for ~p~n", [Route]),
                           '51'();
router(_, _, false)     -> '60'().

format_admins_fn(Params) ->
	{_, Admins} = lists:keyfind(admins, 1, Params),
	Format = "### Name:~n~s~n### Public Key: ~n~p~n",
	[io_lib:format(Format, [N, K]) || #{name := N, key := K} <- Admins].

format_dbs_fn(Params) ->
	{_, DBs} = lists:keyfind(dbs, 1, Params),
	case DBs of
		[] -> io_lib:format("There are no databases yet~n", []);
		_  -> [io_lib:format("=> /db/~s ~s has ~p tables~n", [D, D, length(Ts)]) || {D, Ts} <- lists:sort(DBs)]
	end.

format_tables_fn(Params) ->
	{_, DB}     = lists:keyfind(db, 1, Params),
	{_, Tables} = lists:keyfind(tables, 1, Params),
	[io_lib:format("=> /db/~s/~s ~s~n", [DB, T, T]) || {T, _} <- lists:sort(Tables)].

format_table_details_fn(Params) ->
	{_, DB}       = lists:keyfind(db, 1, Params),
	{_, Table}    = lists:keyfind(table, 1, Params),
	{_, TableDef} = lists:keyfind(tabledef, 1, Params),
	#{cols       := Cols,
	  index      := Idx,
	  index_type := IType} = TableDef,
	[
		"DB:    " ++ DB    ++ "\n",
		"Table: " ++ Table ++ "\n",
		io_lib:format("Index: ~s of type ~s~n", [Idx, IType]),
		"with columns:\n"
	] ++
	[io_lib:format("* ~s of type ~s~n", [K, V]) || {K, V} <- Cols] ++
	[io_lib:format("=> /db/~s go back to the database: ~s~n", [DB, DB])].

table_creation_errors_format_fn(Params) ->
	{_, {Msg, Errors}} = lists:keyfind(errors, 1, Params),
	[io_lib:format("Error: ~s~n", [Msg])] ++
	[io_lib:format("Reason:~n```~n~p~n```~n", [E]) || E <- Errors].

is_admin([],                   _)                  -> false;
is_admin([#{key := K} | _T], #{id := #{key := K}}) -> true;
is_admin([_H | T],             Route)              -> is_admin(T, Route).

'51'() -> [u("51 Welcome to Area 51 👽\r\n")].
'60'() -> [u("60 Criminal Code Section 60 Violation 👮\r\n")].

u(Text) -> unicode:characters_to_binary(Text, utf8).

