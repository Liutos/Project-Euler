-module(pro1).
-export([pro1/1]).

rec(N, Sum, Num) ->
    if
	N >= Num -> Sum;
	0 == N rem 3 -> rec(N+1, Sum+N, Num);
	0 == N rem 5 -> rec(N+1, Sum+N, Num);
	true -> rec(N+1, Sum, Num)
    end.

pro1(Num) ->
    rec(1, 0, Num).
