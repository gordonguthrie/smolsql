# Smol SQL

## Introduction

Smol SQL is designed to be a very small SQL database as a teaching aid. `databases` are represented on disk as directories, `tables` as subdirectories, and `row` as files. Data is persisted in a plain text format to be human readable.


## SQL Dialect

The SQL commands supported are:

```
INSERT INTO table VALUES (val1, val2, val3);
INSERT INTO table (col1, col2, col3...) VALUES (val1, val2, val3...);
SELECT * FROM table;
SELECT col1, col2, col3... FROM table;
SELECT [* | col1, col2, col3, col4...] FROM table WHERE (col1='alice' AND col2='bob'( OR (col3='charlie' AND col4 [=, <, >, =<. >=] 3);
SELECT [* | col1, col2, col3] FROM table1 INNER JOIN table2 ON table1.col1 = table2.col2 WHERE...;
DELETE FROM table WHERE ...;
```

## Credits

It is cut down from the `riak_ql` query language in [Basho's Riak Time Series](https://github.com/basho/riak_ql) DB