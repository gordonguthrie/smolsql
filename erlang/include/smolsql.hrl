%% TODO these types will be improved over the duration of the time series project
-type selection()  :: term().
-type filter()     :: term().
-type operator()   :: term().
-type sorter()     :: term().
-type combinator() :: term().

-record(smolsql_select,
	{
	  'SELECT'      = []    :: [selection() | operator() | combinator()],
	  'FROM'        = <<>>  :: binary() | {list, [binary()]} | {regex, list()},
	  'WHERE'       = []    :: [filter()],
	  'INNER JOIN'  = [],
	  'ON'          = []
	}).

-record(smolsql_insert,
	{
	  'INSERT INTO' = [],
	  'VALUES'      = []
	}).

-record(smolsql_delete,
	{
	  'FROM'        = <<>>  :: binary() | {list, [binary()]} | {regex, list()},
	  'WHERE'       = []    :: [filter()]
	}).
