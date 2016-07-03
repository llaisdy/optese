-module(el_bin_byte).
-include("types.hrl").
-behaviour(element_behaviour).

-export([new/1
	,is_element/1
	,features/1
	,raw/1
	,score/1
	,update/2
	]).

-record(element, {raw, features, score}).

-spec new(binary()) -> element().
new(B) ->
    Bs = bin_to_bytes(B),
    #element{raw=B, features=Bs, score=length(Bs)}.

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

%%%% private

bin_to_bytes(B) ->
    lists:foldl(fun(X, Acc) ->
			case lists:member(X, Acc) of
			    true -> Acc;
			    false -> [X|Acc]
			end
		end, [], binary_to_list(B)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    S = <<"hello there">>,
    Ts = bin_to_bytes(S),
    Exp = #element{raw=S, features=Ts, score=length(Ts)},
    Act = new(S),
    ?_assertEqual(Act, Exp).

update_test_() ->
    #element{score=S1} = E = new(<<"hello">>),
    Ts = bin_to_bytes(<<"llo">>),
    #element{score=S2} = update(E, Ts),
    [?_assertEqual(S1, 4)
    ,?_assertEqual(S2, 2)].

-endif.
