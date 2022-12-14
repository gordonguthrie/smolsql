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
            create_table/3,
            get_table_details/2,
            clean/1
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

create_db(DBName) ->
    gen_server:call(?MODULE, {create_db, DBName}).

list_tables(DBName) ->
    gen_server:call(?MODULE, {list_tables, DBName}).

create_table(TableDef, DBName, Table) ->
    gen_server:call(?MODULE, {create_table, {TableDef, DBName, Table}}).

get_table_details(DBName, TableName) ->
    gen_server:call(?MODULE, {get_table_details, {DBName, TableName}}).

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
    [dump_db(X) || X <- DBs],
    {reply, ok, State};
handle_call(list_dbs, _from, #state{dbs = DBs} = State) ->
    {reply, {ok, DBs}, State};
handle_call({create_db, DBName}, _From, #state{dbroot = DBRoot,
                                               dbs    = DBs} = State) ->
    CleanDB = clean(DBName),
    {NewDBs, Reply} = case lists:keymember(CleanDB, 1, DBs) of
        true  ->
            {DBs, {ok, CleanDB}};
        false ->
            TablesDir = string:join([DBRoot, CleanDB, "tables", "FILES"], "/"),
            ok = filelib:ensure_dir(TablesDir),
            {[{CleanDB, []} | DBs], {ok, CleanDB}}
    end,
    {reply, Reply, State#state{dbs = NewDBs}};
handle_call({list_tables, DBName}, _from, #state{dbs = DBs} = State) ->
    CleanDB = clean(DBName),
    Reply = case lists:keyfind(CleanDB, 1, DBs) of
        false   -> {error, {db_not_defined, CleanDB}};
        {_, Ts} -> {ok,    Ts}
    end,
    {reply, Reply, State};
handle_call({create_table, {TableDef, DBName, TableName}}, _from, State) ->
    #state{dbroot = DBRoot, dbs = DBs} = State,
    CleanDB = clean(DBName),
    CleanTable = clean(TableName),
    {Reply, NewState} = case lists:keyfind(DBName, 1, DBs) of
        false ->
            {{error, {db_not_defined, DBName}}, State};
        {_, TDefs} ->
            case lists:keyfind(CleanTable, 1, TDefs) of
                false ->
                    case parse_and_save(TableDef, DBRoot, CleanDB, CleanTable) of
                        {ok, NormalTDef}  ->
                             PTDef = process_tabledef(NormalTDef),
                             NewTDefs = lists:keystore(CleanTable, 1, TDefs, {CleanTable, PTDef}),
                             NewDBs = lists:keystore(CleanDB, 1, DBs, {CleanDB, NewTDefs}),
                            {{ok, {CleanDB, CleanTable}}, State#state{dbs = NewDBs}};
                        {error, E} ->
                            {{error, {"table not created", E}}, State}
                    end;
                _ ->
                    {{error, {"table not created", ["table already exists"]}}, State}
            end
    end,
    {reply, Reply, NewState};

handle_call({get_table_details, {DBName, TableName}}, _from, State) ->
    #state{dbs = DBs} = State,
    CleanDB = clean(DBName),
    CleanTable = clean(TableName),
    Reply = case lists:keyfind(DBName, 1, DBs) of
        false ->
            {error, {db_not_defined, CleanDB}};
        {_, TDefs} ->
            case lists:keyfind(CleanTable, 1, TDefs) of
                false ->
                    {error, {table_not_defined, {CleanDB, CleanTable}}};
                {_, TDef} ->
                    {ok, [{db,       CleanDB},
                          {table,    CleanTable},
                          {tabledef, TDef}]}
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
    [io:format("  * ~s of type ~s~n", [C, CType]) || {C, CType} <- Cols],
    dump_tables(T).

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
    {ok, TableDef} = file:consult(H),
    Def = process_tabledef(TableDef),
    read_tabledefs(T, [{TableName, Def} | Acc]).

process_tabledef(TableDef) ->
    {_, [{Type, Index}]} = lists:keyfind(index, 1, TableDef),
    {_, Cols}            = lists:keyfind(cols, 1, TableDef),
    #{index => Index, index_type => Type, cols => Cols}.

parse_and_save(TableDef, DBRoot, DB, Table) ->
    Lines = normalise_def(TableDef),
    case parse(Lines, [], [], []) of
        {ok, ParsedTableDef} ->
            File = filename:join([DBRoot, DB, Table ++ ".tabledef"]),
            ok = write_terms(File, ParsedTableDef),
            {ok, ParsedTableDef};
        {errors, E} ->
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
    ok = file:write_file(FileName, Text).
