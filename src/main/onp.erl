%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2018 12:35
%%%-------------------------------------------------------------------
-module(onp).
-author("mateusz").

%% API
-export([onp/1,count/1]).
-import(string,[tokens/2]).
-import(lists,[map/2]).

parse(Expression) ->
  map(fun convert/1,tokens(Expression, " ")).

to_number(Num) ->
  try list_to_float(Num)
  catch
    error:badarg ->
      list_to_integer(Num)
  end.

convert("+") -> fun (X,Y) -> X+Y end;
convert("-") -> fun (X,Y) -> X-Y end;
convert("*") -> fun (X,Y) -> X*Y end;
convert("/") -> fun (X,Y) -> X/Y end;
convert("^") -> fun math:pow/2;
convert("sqrt") -> fun math:sqrt/1;
convert("sin") -> fun math:sin/1;
convert("cos") -> fun math:cos/1;
convert("tan") -> fun math:tan/1;
convert("pi") -> fun math:pi/0;
convert(Num) -> to_number(Num).

count([Result])->Result;
count([Fun | Tail]) when is_function(Fun)->
  count([ Fun() | Tail]);
count([Num, Fun| Tail]) when is_function(Fun)->
  count([ Fun(Num) | Tail]);
count([Num1, Num2 , Fun | Tail]) when is_function(Fun)->
  count([ Fun(Num1, Num2) | Tail]).

onp(Expression) when length(Expression)>1 ->
  (count(parse(Expression))).

% 1 + 2 * 3 - 4 / 5 + 6
% 1 2 3 * + 4 5 / - 6 +

% 1 + 2 + 3 + 4 + 5 + 6 * 7
% 1 2 + 3 + 4 + 5 + 6 5 * +

% ( (4 + 7) / 3 ) * (2 - 19)
% 4 7 + 3 / 2 19 - *

% 17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1
% 17 31 4 + * 26 15 - 2 * 22 - / 1 -


