-module(el_string_trigram).
-include("types.hrl").
-behaviour(element_behaviour).

-export([new/1
	,is_element/1
	,features/1
	,raw/1
	,score/1
	,update/2  %% remove features & rescore element
	]).

-record(element, {raw, features, score}).

-spec new(string()) -> element().
new(S) ->
    Ts = liga_util:string_to_trigrams(S),
    #element{raw=S, features=Ts, score=length(Ts)}.

is_element(E) ->
    is_record(E, element).

-spec features(element()) -> list(feature()).
features(#element{features=X}) -> X.

-spec raw(element()) -> string().
raw(#element{raw=X})-> X.

-spec score(element()) -> non_neg_integer().
score(#element{score=X}) -> X.

-spec update(element(), list(feature())) -> element().
update(#element{features=Fs1} = E, Fs2) ->
    Fs3 = lists:filter(fun(F) ->
			       not lists:member(F, Fs2)
		       end, Fs1),
    E#element{features=Fs3, score=length(Fs3)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    S = "hello there",
    Ts = liga_util:string_to_trigrams(S),
    Exp = #element{raw=S, features=Ts, score=length(Ts)},
    Act = new(S),
    ?_assertEqual(Act, Exp).

update_test_() ->
    #element{score=S1} = E = new("hello"),
    Ts = liga_util:string_to_trigrams("llo"),
    #element{score=S2} = update(E, Ts),
    [?_assertEqual(S1, 5)
    ,?_assertEqual(S2, 3)].

-endif.
