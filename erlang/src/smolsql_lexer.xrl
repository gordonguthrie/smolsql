%%% -*- mode: erlang -*-
%%% @doc       Lexer for the SmolSQL.
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Definitions.

AND = (A|a)(N|n)(D|d)
AS = (A|a)(S|s)
DELETE = (D|d)(E|e)(L|l)(E|e)(T|t)(E|e)
FROM = (F|f)(R|r)(O|o)(M|m)
INNER = (I|i)(N|n)(N|n)(E|e)(R|r)
INNER_JOIN = (I|i)(N|n)(N|n)(E|e)(R|r)\s(J|j)(O|o)(I|i)(N|n)
INSERT_INTO = (I|i)(N|n)(S|s)(E|e)(R|r)(T|t)\s(I|i)(N|n)(T|t)(O|o)
JOIN = (J|j)(O|o)(I|i)(N|n)
ON = (O|o)(N|n)
OR = (O|o)(R|r)
SELECT = (S|s)(E|e)(L|l)(E|e)(C|c)(T|t)
VALUES = (V|v)(A|a)(L|l)(U|u)(E|e)(S|s)
WHERE = (W|w)(H|h)(E|e)(R|r)(E|e)

DATETIME = ('[0-9a-zA-Z\s:\-\.]*')

REGEX = (/[^/][a-zA-Z0-9\*\.]+/i?)

QUOTED = ("(.*(\")*)")

WHITESPACE = ([\000-\s]*)

INTNUM   = (\-*[0-9]+)
FLOATDEC = (\-*([0-9]+)?\.[0-9]+)
FLOATSCI = (\-*([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

APPROXMATCH = (=\~)
EQ          = (=)
GT          = (>)
LT          = (<)
NE          = (<>)
NOMATCH     = (!=)
NOTAPPROX   = (!\~)
OPEN        = \(
CLOSE       =\)

PLUS  = (\+)
MINUS = (\-)
TIMES = (\*)
DIV   = (/)

COMMA = (,)

Rules.

{AND} : {token, {and_, TokenChars}}.
{DELETE} : {token, {delete, TokenChars}}.
{FROM} : {token, {from, TokenChars}}.
{INNER} : {token, {inner, TokenChars}}.
{INNER_JOIN} : {token, {inner_join, TokenChars}}.
{INSERT_INTO} : {token, {insert_into, TokenChars}}.
{INT} : {token, {int_type, TokenChars}}.
{JOIN} : {token, {join, TokenChars}}.
{ON} : {token, {on, TokenChars}}.
{OR} : {token, {or_, TokenChars}}.
{SELECT} : {token, {select, TokenChars}}.
{VALUES} : {token, {values, TokenChars}}.
{VARCHAR} : {token, {varchar, TokenChars}}.
{WHERE} : {token, {where, TokenChars}}.

{INTNUM}   : {token, {int, list_to_integer(TokenChars)}}.
{FLOATDEC} : {token, {float, list_to_float(TokenChars)}}.
{FLOATSCI} : {token, {float, list_to_float(TokenChars)}}.

{EQ}          : {token, {eq,     TokenChars}}.
{APPROXMATCH} : {token, {approx, TokenChars}}.
{GT}          : {token, {gt,        TokenChars}}.
{LT}          : {token, {lt,        TokenChars}}.
{NE}          : {token, {ne,        TokenChars}}.
{NOMATCH}     : {token, {nomatch,   TokenChars}}.
{NOTAPPROX}   : {token, {notapprox,   TokenChars}}.

{OPEN}  :  {token, {openb,  TokenChars}}.
{CLOSE} :  {token, {closeb, TokenChars}}.

{PLUS}  : {token, {plus,       TokenChars}}.
{MINUS} : {token, {minus,      TokenChars}}.
{TIMES} : {token, {maybetimes, TokenChars}}.
{DIV}   : {token, {div_,       TokenChars}}.

{DATETIME} : {token, fix_up_date(TokenChars)}.

{QUOTED} : {token, {quoted, strip_quoted(TokenChars)}}.

{REGEX} : {token, {regex, TokenChars}}.

{COMMA} : {token, {comma, TokenChars}}.

{WHITESPACE} : {token, {whitespace, TokenChars}}.

\n : {end_token, {'$end'}}.

.  : {token, {chars, TokenChars}}.

Erlang code.

-export([
         get_tokens/1
        ]).

-include("../tests/smolsql.xrl.tests").

get_tokens(X) ->
    Toks = lex(X),
    post_process(Toks).

post_process(X) ->
    post_p(X, []).

% filter out the whitespaces at the end
post_p([], Acc) ->
    [{Type, X} || {Type, X} <- lists:reverse(Acc), Type =/= whitespace];
% when you've merged two items hoy them back on the list
% so they can continue to sook up chars
post_p([{Word1, W1}, {Word2, W2} | T], Acc) when Word1 =:= chars   orelse
                                                 Word1 =:= select  orelse
                                                 Word1 =:= from    orelse
                                                 Word1 =:= and_    orelse
                                                 Word1 =:= or_     orelse
                                                 Word1 =:= on      orelse
                                                 Word1 =:= delete  orelse
                                                 Word1 =:= inner   orelse
                                                 Word1 =:= join,
                                                 Word2 =:= chars   orelse
                                                 Word2 =:= select  orelse
                                                 Word2 =:= from    orelse
                                                 Word2 =:= and_    orelse
                                                 Word2 =:= or_     orelse
                                                 Word2 =:= on      orelse
                                                 Word2 =:= delete  orelse
                                                 Word2 =:= inner   orelse
                                                 Word2 =:= join    ->
    post_p([{chars, W1 ++ W2} | T], Acc);
post_p([H | T], Acc) ->
    post_p(T, [H | Acc]).

lex(String) -> {ok, Toks, 1} = string(String),
               Toks.

fix_up_date(Date) ->
    Date2 = string:strip(Date, both, $'), %'
    Date3 = string:strip(Date2),
    case dh_date:parse(Date3) of
        {error, bad_date} -> {chars, Date2};
        Date4             -> {datetime, Date4}
    end.

strip_quoted(Date) ->
    Date2 = string:strip(Date, both, $"), %"
    string:strip(Date2).
