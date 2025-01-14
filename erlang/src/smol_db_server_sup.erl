%%%-------------------------------------------------------------------
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------

-module(smol_db_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    Templates  = {belka_templates, {belka_templates, start_link, []},
                  permanent, 5000, worker, [belka_templates]},
    Engine     = {smol_sql_engine, {smol_sql_engine, start_link, []},
                  permanent, 5000, worker, [smol_sql_engine]},
    ChildSpecs = [
    				Templates,
                    Engine
    			 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
