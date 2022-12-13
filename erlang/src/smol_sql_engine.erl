%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(smol_sql_engine).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% OTP API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% API
-export([
            list_dbs/0,
            create_db/1,
            list_tables/1,
            create_table/3
        ]).

% Debugging
-export([
            dump/0
        ]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include("../tests/smol_sql_engine.tests").
-endif.

-record(state, {dbroot = none, dbs = []}).

-define(ACCUMULATOR, []).

% OTP API

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

%% API

list_dbs() ->
    gen_server:call(?MODULE, list_dbs).

create_db(DB) ->
    gen_server:call(?MODULE, {create_db, DB}).

list_tables(DB) ->
    gen_server:call(?MODULE, {list_tables, DB}).

create_table(TableDef, DB, Table) ->
    gen_server:call(?MODULE, {create_table, {TableDef, DB, Table}}).

% Debugging
dump() ->
    gen_server:call(?MODULE, dump).

% Callbacks

init(_Args) ->
    true = register(?MODULE, self()),
    {ok, DBRoot} = application:get_env(smol_sql_engine, dbroot),
    {ok, DBs} = file:list_dir(DBRoot),
    DBsAndTables = read_dbs(DBs, DBRoot, ?ACCUMULATOR),
    {ok, #state{dbroot = DBRoot, dbs = DBsAndTables}}.

handle_call(dump, _from, #state{dbroot = DBRoot, dbs = DBs} = State) ->
    io:format("The directory root of the databases is ~p~n", [DBRoot]),
    [dump_db(X) || X <- DBs],
    {reply, ok, State};
handle_call(list_dbs, _from, #state{dbs = DBs} = State) ->
    {reply, {ok, DBs}, State};
handle_call({create_db, DBName}, _From, #state{dbroot = DBRoot,
                                               dbs    = DBs} = State) ->
    CleanDB = clean(DBName),
    {NewDBs, Reply} = case proplists:is_defined(CleanDB, DBs) of
        true  ->
            io:format("db already created~n"),
            {DBs, {ok, CleanDB}};
        false ->
            io:format("creating db and tables"),
            TablesDir = string:join([DBRoot, CleanDB, "tables", "FILES"], "/"),
            ok = filelib:ensure_dir(TablesDir),
            {[{CleanDB, []} | DBs], {ok, CleanDB}}
    end,
    {reply, Reply, State#state{dbs = NewDBs}};
handle_call({list_tables, DB}, _from, #state{dbs = DBs} = State) ->
    CleanDB = clean(DB),
    Reply = case proplists:get_value(CleanDB, DBs) of
        undefined -> {error, {db_not_defined, CleanDB}};
        Ts         -> {ok,    Ts}
    end,
    {reply, Reply, State};
handle_call({create_table, {TableDef, DB, TableName}}, _from, State) ->
    #state{dbroot = DBRoot, dbs = DBs} = State,
    CleanDB = clean(DB),
    CleanTable = clean(TableName),
    Reply = case proplists:get_value(DB, DBs) of
        undefined ->
            {error, {db_not_defined, DB}};
        _Ts ->
            case parse_and_save(TableDef, DBRoot, CleanDB, CleanTable) of
                ok         -> {ok, CleanTable};
                {error, E} -> {error, {"table not created", E}}
            end
    end,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    io:format("in handle call with ~p~n", [Request]),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
clean(String) -> clean(String, []).

clean([], Acc)       -> lists:reverse(Acc);
clean([H | T],  Acc) when (H >= 65 andalso H =< 90)  orelse
                          (H >= 97 andalso H =< 122) orelse
                          (H >= 48 andalso H =< 57)  -> clean(T, [H | Acc]);
clean([_H | T], Acc) -> clean(T, Acc).

dump_db({DB, Tables}) ->
    io:format("----------------~n"),
    io:format("Database: ~s~n", [DB]),
    case Tables of
        [] -> io:format("~s has no tables~n", [DB]);
        _  -> io:format("Tables:~n"),
              dump_tables(Tables)
    end,
    io:format("----------------~n").

dump_tables([]) -> ok;
dump_tables([H | T]) ->
    {Table, Def} = H,
    #{index := Idx, index_type := IType, cols := Cols} = Def,
    io:format("Table: ~s~n", [Table]),
    io:format("* index: ~s of type ~s~n", [Idx, IType]),
    io:format("* the colums are:~n"),
    [io:format("  * ~s of type ~s~n", [C, CType]) || {C, CType} <- Cols].

read_dbs([],     _DBRoot, Acc) -> lists:sort(Acc);
read_dbs([H | T], DBRoot, Acc) ->
    TableDefFiles = filename:join([DBRoot, H, "*.tabledef"]),
    NewAcc = case filelib:wildcard(TableDefFiles) of
        []   -> {H, []};
        TDFs -> TableDefs = read_tabledefs(TDFs, []),
                {H, TableDefs}
    end,
    read_dbs(T, DBRoot, [NewAcc | Acc]).

read_tabledefs([], Acc) -> Acc;
read_tabledefs([H | T], Acc) ->
    TableName = filename:basename(H, ".tabledef"),
    {ok, TD} = file:consult(H),
    io:format("TD is ~p~n", [TD]),
    [{Type, Index}] = proplists:get_value(index, TD),
    Cols            = proplists:get_value(cols, TD),
    Def = #{index => Index, index_type => Type, cols => Cols},
    read_tabledefs(T, [{TableName, Def} | Acc]).

parse_and_save(TableDef, DBRoot, DB, Table) ->
    Lines = normalise_def(TableDef),
    case parse(Lines, [], [], []) of
        {ok, ParsedTableDef} ->
            File = filename:join([DBRoot, DB, Table ++ ".tabledef"]),
            ok = write_terms(File, ParsedTableDef);
        {error, E} ->
            {error, E}
    end.

parse([], [], [], []) -> {errors, ["no table definition"]};
parse([], [], [],  _) -> {errors, ["no index definition"]};
parse([], [],  _, []) -> {errors, ["no column definitions"]};
parse([], [], Index, Cols) ->
    case {is_index_valid(Index), are_cols_valid(Cols)} of
        {true,               true} -> {ok, [{index, Index}, {cols, Cols}]};
        {{false, E1}, {false, E2}} -> {errors, E1 ++ E2};
        {{false, E},  true}        -> {errors, E};
        {true,        {false, E}}  -> {errors, E}
    end;
parse([], Errs, _, _) -> {errors, Errs};
parse([{I, _} = H | T], Errs, Index, Cols) when I == "autoindex" orelse
                                                I == "index" ->
    parse(T, Errs, [H | Index], Cols);
parse([{_, Ty} = H | T], Errs, Index, Cols) when Ty == "varchar" orelse
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
    {ColNames, _Types} = lists:unzip(Cols),
    UniqNames = lists:usort(ColNames),
    case length(UniqNames) of
        NoCols -> true;
        _      -> {false, ["duplicated fields"]}
    end.

normalise_def(Def) ->
    Lines = [string:split(X, ":") || X <- string:tokens(Def, "\n")],
    [{clean(K), clean(V)} || [K, V] <- Lines].

write_terms(FileName, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    io:format("FileName is ~p~n", [FileName]),
    ok = file:write_file(FileName, Text).
