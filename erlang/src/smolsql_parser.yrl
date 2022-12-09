%% -*- erlang -*-
%%% @doc       Parser for SmolSQLx.
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Nonterminals

Statement
Query
Select
Delete
Insert
Bucket
Buckets
Field
Fields
Word
Where
Cond
Conds
Comp
Logic
Val
Val2
Vals
Vals2
.

Terminals

insert_into
select
from
limit
where
and_
 or_
on
delete
inner_join
int
float
eq
gt
lt
ne
nomatch
approx
notapprox
openb
closeb
plus
minus
maybetimes
div_
comma
chars
values
quoted
datetime
varchar
regex
.

Rootsymbol Statement.
Endsymbol '$end'.

Statement -> Query : convert('$1').
Statement -> Insert : '$1'.
Statement -> Delete : convert('$1').

Insert -> insert_into Bucket openb Fields closeb values openb Vals2 closeb : make_insert('$2', '$4', '$8').

Query -> Select inner_join Bucket on Field eq Field : add_inner('$1', '$3', '$5', '$7').
Query -> Select limit int : add_limit('$1', '$2', '$3').
Query -> Select           : '$1'.

Select -> select Fields from Buckets Where : make_clause('$1', '$2', '$3', '$4', '$5').
Select -> select Fields from Buckets       : make_clause('$1', '$2', '$3', '$4').
Where -> where Conds : make_where('$1', '$2').

Fields -> Fields comma Field : make_list('$1', '$3').
Fields -> Field              : make_list('$1').

Field -> Word                : '$1'.
Field -> maybetimes          : '$1'.

Buckets -> Buckets comma Bucket : make_list('$1', '$3').
Buckets -> Bucket               : '$1'.

Bucket -> Word   : '$1'.
Bucket -> regex  : '$1'.
Bucket -> quoted : '$1'.

Word -> Word chars : concatenate('$1', '$2').
Word -> chars      : process('$1').

Conds -> openb Conds closeb             : make_expr('$2').
Conds -> Conds Logic Cond               : make_expr('$1', '$2', '$3').
Conds -> Conds Logic openb Conds closeb : make_expr('$1', '$2', '$4').
Conds -> Cond                           : '$1'.

Cond -> Vals Comp Vals : make_expr('$1', '$2', '$3').

Vals2 -> Vals2 comma Val2 : make_list('$1', '$3').
Vals2 -> Val2 : make_list('$1').

Val2 -> Val  : '$1'.
Val2 -> Word : '$1'.

Vals -> Vals plus       Val : make_expr('$1', '$2', '$3').
Vals -> Vals minus      Val : make_expr('$1', '$2', '$3').
Vals -> Vals maybetimes Val : make_expr('$1', '$2', '$3').
Vals -> Vals div_       Val : make_expr('$1', '$2', '$3').
Vals -> regex               : '$1'.
Vals -> Val                 : '$1'.
Vals -> Word                : '$1'.

Val -> int chars : add_unit('$1', '$2').
Val -> int       : '$1'.
Val -> float     : '$1'.
Val -> datetime  : '$1'.
Val -> varchar   : '$1'.
Val -> quoted    : make_word('$1').

Logic -> and_ : '$1'.
Logic -> or_  : '$1'.

Comp -> approx    : '$1'.
Comp -> eq        : '$1'.
Comp -> gt        : '$1'.
Comp -> lt        : '$1'.
Comp -> ne        : '$1'.
Comp -> nomatch   : '$1'.
Comp -> notapprox : '$1'.

%% DELETE
%% Section 14.9 of the SQL Foundation Document
%% We only implement <delete statement: searched>
Delete -> delete from Bucket Where : make_delete('$3', '$4').
Delete -> delete from Bucket       : make_delete('$3', {where, []}).

Erlang code.

-record(outputs,
        {
          type       = [] :: select | create,
          buckets    = [],
          fields     = [],
	inner_join = [],
	  on         = [],
          limit      = [],
          where      = [],
          ops        = []
         }).

-include("smolsql.hrl").

%% export the return value function to prevent xref errors
%% this fun is used during the parsing and is marked as
%% unused/but not to be exported in the yecc source
%% no way to stop rebar borking on it AFAIK
-export([
	 return_error/2
	 ]).

-ifdef(TEST).
-include("smolsql.yrl.tests").
-endif.

make_insert({word, Bucket}, {list, Fields}, {list, Vals}) ->
    Len1 = length(Fields),
    Len2 = length(Vals),
    case Len1 of
	Len2 -> #smolsql_insert{'INSERT INTO' = Bucket,
				            'VALUES'      = lists:zip(Fields, Vals)}
		    ;
	_ -> exit("field list doesn't match values list")
    end.


convert(#outputs{type       = select,
		         buckets    = B,
		         fields     = F,
		         inner_join = II,
		         on         = O,
		         where      = W}) ->
	#smolsql_select{'SELECT'     = F,
			        'FROM'       = B,
			        'WHERE'      = W,
			        'INNER JOIN' = II,
			        'ON'         = O};
convert(#outputs{type    = delete,
                 buckets = B,
                 where   = W}) ->
    #smolsql_delete{'FROM'  = B,
                    'WHERE' = W}.

process({chars, A}) ->
    {word, A}.

concatenate({word, A}, {chars, B}) ->
    {word, A ++ B}.

make_clause(A, B, C, D) -> make_clause(A, B, C, D, {where, []}).

make_clause({select, _A}, {_, B}, {from, _C}, {Type, D}, {_, E}) ->
    Type2 = case Type of
                list   -> list;
                word   -> string;
                quoted -> string;
                regex  -> regex
            end,
    Bucket = case Type2 of
		 string -> D;
		 list   -> {Type2, D};
		 regex  -> {Type2, D}
	     end,
    _O = #outputs{type    = select,
                  fields  = [[X] || X <- B],
                  buckets = Bucket,
                  where   = E
                 }.

add_inner(A, {word, B}, {word, C}, {word, D}) ->
    A#outputs{inner_join = B,
	          on         = {C, D}}.

add_limit(A, _B, {int, C}) ->
    A#outputs{limit = C}.

make_expr({_, A}, {B, _}, {Type, C}) ->
    B1 = case B of
             and_      -> and_;
             or_       -> or_;
             plus      -> '+';
             minus     -> '-';
             maybetime -> '*';
             div_      -> '/';
             gt        -> '>';
             lt        -> '<';
             eq        -> '=';
             ne        -> '<>';
             approx    -> '=~';
             notapprox -> '!~';
             nomatch   -> '!='
         end,
    C2 = case Type of
             conditional -> C;
             _           -> {Type, C}
         end,
    {conditional, {B1, A, C2}}.

make_word({quoted, Q}) -> {word, Q}.

make_where({where, A}, {conditional, B}) ->
    NewB = remove_conditionals(B),
    {A, [NewB]}.

				     
remove_conditionals({conditional, A}) ->
    A;
remove_conditionals({A, B, C}) ->
    {A, remove_conditionals(B), remove_conditionals(C)};
remove_conditionals(A) ->
    A.

add_unit({Type, A}, {chars, U}) when U =:= "s" -> {Type, A};
add_unit({Type, A}, {chars, U}) when U =:= "m" -> {Type, A*60};
add_unit({Type, A}, {chars, U}) when U =:= "h" -> {Type, A*60*60};
add_unit({Type, A}, {chars, U}) when U =:= "d" -> {Type, A*60*60*24}.

make_list({maybetimes, A}) -> {list, [A]};
make_list({word,       A}) -> {list, [A]};
make_list(A)               -> {list, [A]}.

make_list({list, A}, {_, B}) -> {list, A ++ [B]};
make_list({_,    A}, {_, B}) -> {list, [A, B]}.

make_expr(A) ->
    {conditional, A}.

make_delete({word, Bucket}, {where, W}) ->
   #outputs{type    = delete,
            buckets = Bucket,
            where   = W}.
