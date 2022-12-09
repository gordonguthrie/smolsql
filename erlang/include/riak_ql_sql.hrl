%% TODO these types will be improved over the duration of the time series project
-type selection()  :: term().
-type filter()     :: term().
-type operator()   :: term().
-type sorter()     :: term().
-type combinator() :: term().

-record(riak_sql_v1,
	{
	  'SELECT'      = []    :: [selection() | operator() | combinator()],
	  'FROM'        = <<>>  :: binary() | {list, [binary()]} | {regex, list()},
	  'WHERE'       = []    :: [filter()],
	  'ORDER BY'    = []    :: [sorter()],
	  'INNER JOIN'  = [],
	  'ON'          = []
	}).

-record(riak_sql_insert_v1,
	{
	  'INSERT INTO' = [],
	  'VALUES'      = []
	}).
