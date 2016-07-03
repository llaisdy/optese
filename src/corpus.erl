-module(corpus).
-include("types.hrl").

-record(corpus, {elements=[], element_module, n_to_get=1, min_cover=1}).

-export([new/2
	,is_corpus/1
	,elements/1
	,size/1
	,features/2
	,best_rest/1
	,optese/1
	]).

-spec new(list(element()), module()) -> corpus().
new(Es, Mod) ->
    #corpus{elements=Es, element_module=Mod}.

is_corpus(C) ->
    is_record(C, corpus).

-spec size(corpus()) -> non_neg_integer().
size(#corpus{elements=Es}) ->
    length(Es).

-spec elements(corpus()) -> list(element()).
elements(#corpus{elements=Es}) ->
    Es.

-spec features(corpus() ,feature_sort_spec()) ->list(feature_count()).
features(#corpus{elements=Es, element_module=Mod}, Spec) ->
    FDict = feature_dict(Es, Mod),
    sort(dict:to_list(FDict), Spec).

-spec best_rest(corpus()) -> {element(), corpus()}.
best_rest(#corpus{elements=[H|T], element_module=Mod}=C) ->
    {Best, _BestScore, RestRaw} = best_rest_raw(H, T, Mod),
    RestRes = rescore_elements(RestRaw, Mod:features(Best), Mod),
    case RestRes of
	[] -> {Best, empty};
	_ -> {Best, C#corpus{elements=RestRes, element_module=Mod}}
    end.

-spec optese(corpus()) -> list(element()).
optese(C) ->
    {Best, Rest} = best_rest(C),
    case Rest of
	empty -> [Best];
	_     -> [Best | optese(Rest)]
    end.

%%%% private

best_rest_raw(H, T, Mod) ->
    lists:foldl(fun(E, {BestSoFar, BestScoreSoFar, RestSoFar}) ->
			S = Mod:score(E),
			case S > BestScoreSoFar of
			    true ->
				{E, S, [BestSoFar|RestSoFar]};
			    false ->
				{BestSoFar, BestScoreSoFar, [E|RestSoFar]}
			end
		end, {H, Mod:score(H), []}, T).

feature_dict(Es, Mod) ->
    lists:foldl(fun(E, Acc1) ->
			lists:foldl(fun(F, Acc2) ->
					    update(F, Acc2)
				    end, 
				    Acc1, Mod:features(E))
		end,
		dict:new(),
		Es).

rescore_elements(Es, Fs, Mod) ->
    lists:foldl(fun(E, Acc) ->
			N = Mod:update(E, Fs),
			case Mod:score(N) of
			    0 -> Acc;
			    _ -> [N|Acc]
			end
		end, [], Es).

sort(FList, {freq, desc}) ->
    lists:sort(fun({_,X}, {_,Y}) -> X > Y end, FList);

sort(FList, {feat, desc}) ->
    lists:sort(fun({X,_}, {Y,_}) -> X > Y end, FList).

update(Key, Dict) ->
    dict:update(Key, fun(Val) -> Val + 1 end, 1, Dict).

%%%% tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    Ss = ["qwe rty","rty asd","asd fgh","fgh zxc","zxc vbn"],
    Es = lists:map(fun el_string_trigram:new/1, Ss),
    Exp = #corpus{elements=Es, element_module=el_string_trigram},
    Act = new(Es, el_string_trigram),
    ?_assertEqual(Exp, Act).

-endif.
