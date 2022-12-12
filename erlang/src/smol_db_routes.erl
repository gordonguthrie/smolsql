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
			format_tables_fns/1
		]).

-ifdef(TEST).
-include("../tests/smol_db_routes.tests").
-endif.

router(Route) ->
	{ok, DBRoot} = application:get_env(smol_db_server, dbroot),
    {ok, Admins} = application:get_env(smol_db_server, admins),
    IsAdmin = is_admin(Admins, Route),
    router(Route, Admins, IsAdmin, DBRoot).

%% the route for the home page
router(#{path := []}, Admins, _, DBRoot) ->
	{ok, DBs} = file:list_dir(DBRoot),
    Params = [{admins, Admins}, {dbs, DBs}],
    belka_templates:render(smol_db_routes, "frontpage", Params);

%% create new tables in a database
router(#{path := ["dbs", "create"], querykvs := []}, _, true, _DBRoot) ->
	["10 what is the name of the new database?\r\n"];
router(#{path := ["dbs", "create"], querykvs := [{DBName, true}]}, _, true, DBRoot) ->
	CleanDB = clean(DBName),
	TablesDir = string:join([DBRoot, CleanDB, "tables"], "/"),
	ok = filelib:ensure_dir(TablesDir),
	["30 /db/" ++ CleanDB ++ "\r\n"];
router(#{path := ["dbs", DBName, "create"], querykvs := [{TableName, true}]}, _, true, _DBRoot) ->
	CleanDB = clean(DBName),
	CleanTable = clean(TableName),
	io:format("trying to create a table called ~p in ~p~n", [CleanTable, CleanDB]),
	belka_templates:render("create_table", [{db, CleanDB}, {table, CleanTable}]);
router(#{path := ["dbs", DBName, "create" ], querykvs := []}, _, true, _DBRoot) ->
	["10 what is the name of the new table in " ++ DBName ++ "?\r\n"];
router(#{path := ["dbs", DBName, "create", TableName], querykvs := []}, _, true, _DBRoot) ->
	CleanDB = clean(DBName),
	CleanTable = clean(TableName),
    io:format("posting sql to ~p~n", [CleanDB]),
    [
        "10 Post SQL to create table:" ++ CleanTable ++ " in db:" ++ CleanDB ++ "\r\n"
    ];
router(#{path := ["dbs", DBName, "create", TableName], querykvs := [{TableDef, true}]}, _, true, DBRoot) ->
    io:format("posting table def ~p to create ~p on ~p~n", [TableDef, TableName, DBName]),
	CleanDB = clean(DBName),
	CleanTable = clean(TableName),
	case parse_and_save(TableDef, DBRoot, CleanDB, CleanTable) of
		ok          -> ["20 text/gemini\r\ntable created\r\n"];
		{error, _E} -> ["20 text/gemini\r\ntable not created\r\n"]
	end;

%% show all the tables that exist
router(#{path := ["db", DBName], querykvs := []}, _, true, DBRoot) ->
	DBDir = string:join([DBRoot, "tables", DBName], "/"),
	CleanDB = clean(DBName),
    case file:list_dir(DBDir) of
    	{ok, []}        -> belka_templates:render("tables_none", [{db, CleanDB}]);
    	{ok, Tables}    -> Params = [{db, DBName}, {tables, Tables}],
    					   belka_templates:render(smol_db_routes, "tables_exist", Params);
    	{error, enoent} -> belka_templates:render("tables_none", [{db, CleanDB}])
    end;

%% handle not founds and log in attempts that fail
router(Route, _, true, _)  -> io:format("in route 51 for ~p~n", [Route]),
                              '51'();
router(_, _, false, _)     -> '60'().

parse_and_save(TableDef, _DBRoot, _DB, _Table) ->
	Lines = normalise_def(TableDef),
	io:format("Lines are ~p~n", [Lines]),
	Ret = parse(Lines, [], [], []),
	io:format("Ret is ~p~n", [Ret]),
	{error, banjil}.

parse([], [], [], []) -> {errors, ["no table definition"]};
parse([], [], [],  _) -> {errors, ["no index definition"]};
parse([], [],  _, []) -> {errors, ["no column definitions"]};
parse([], [], Index, Cols) ->
	case {is_index_valid(Index), are_cols_valid(Cols)} of
		{true, true} -> {index, Index, cols, Cols};
		{{false, E1}, {false, E2}} -> {errors, E1 ++ E2};
		{{false, E},  true}        -> {errors, E};
		{true,        {false, E}}  -> {errors, E}
	end;
parse([], Errs, _, _) -> {errors, Errs};
parse([{I, _} = H | T], Errs, Index, Cols) when I == "autoindex" orelse
   									   		    I == "index" ->
   	parse(T, Errs, [H | Index], Cols);
parse([{F, Ty} = H | T], Errs, Index, Cols) when Ty == "varchar" orelse
   									   			 Ty == "integer" orelse
   										   		 Ty == "float"   orelse
   										   		 Ty == "date"    orelse
   										   		 Ty == "boolean" ->
   	parse(T, Errs, Index, [H | Cols]);
parse([H | T], Errs, Index, Cols) ->
	NewErrs = [{invalid_type, H} | Errs],
	parse(T, NewErrs, Index, Cols).

is_index_valid(Idx) ->
	case length(Idx) of
		1 -> true;
		N -> {false, [{wrong_no_of_indices, N}]}
	end.

are_cols_valid(Cols) ->
	NoCols = length(Cols),
	{ColNames, Types} = lists:unzip(Cols),
	UniqNames = lists:usort(ColNames),
	case length(UniqNames) of
		NoCols -> true;
		_      -> {false, ["duplicated fields"]}
	end.

normalise_def(Def) ->
	Lines = [string:split(X, ":") || X <- string:tokens(Def, "\n")],
	[{clean(K), clean(V)} || [K, V] <- Lines].

format_admins_fn(Params) ->
	Admins = proplists:get_value(admins, Params),
	Format = "### Name:~n~s~n### Public Key: ~n~p~n",
	[io_lib:format(Format, [N, K]) || #{name := N, key := K} <- Admins].

format_dbs_fn(Params) ->
	DBs = proplists:get_value(dbs, Params),
	case DBs of
		[] -> "There are no databases yet\r\n";
		_  -> [io_lib:format("=> /db/~s ~s~n", [D, D]) || D <- DBs]
	end.

format_tables_fns(Params) ->
	DB     = proplists:get_value(db, Params),
	Tables = proplists:get_value(tables, Params),
	[io_lib:format("=> /db/~s/~s ~s~n", [DB, T, T]) || T <- Tables].

is_admin([],                   _)                  -> false;
is_admin([#{key := K} | _T], #{id := #{key := K}}) -> true;
is_admin([_H | T],             Route)              -> is_admin(T, Route).

'51'() -> [u("51 Welcome to Area 51 👽\r\n")].
'60'() -> [u("60 Criminal Code Section 60 Violation 👮\r\n")].

u(Text) -> unicode:characters_to_binary(Text, utf8).

clean(String) -> clean(String, []).

clean([], Acc)       -> lists:reverse(Acc);
clean([H | T],  Acc) when (H >= 65 andalso H =< 90)  orelse
                          (H >= 97 andalso H =< 122) orelse
                          (H >= 48 andalso H =< 57) -> clean(T, [H | Acc]);
clean([_H | T], Acc) -> clean(T, Acc).


