-module(optese_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0
	,init_per_suite/1, end_per_suite/1
	,init_per_group/2, end_per_group/2
	,init_per_testcase/2, end_per_testcase/2
	 ]).

-export([optese_features/1
	,best_rest/1
	,demo/1
	]).

all() ->
    [{group, element_binary_byte}
    ,{group, element_string_trigram}
    ,{group, it_IT}
    ].

groups() ->
    [{element_binary_byte, [], tests()}
    ,{element_string_trigram, [], tests()}
    ,{it_IT, [], [demo]}
    ].

tests() ->
    [optese_features
    ,best_rest
    ].

%%%% set up

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(element_binary_byte,Config) ->
    fixture_data(el_bin_byte, Config);
init_per_group(element_string_trigram,Config) ->
    fixture_data(el_string_trigram, Config);
init_per_group(_,Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%% tests

optese_features(Config) ->
    C = ?config(corpus, Config),
    M = ?config(element_module, Config),
    Es = corpus:elements(C),
    O = corpus:optese(C),
    AllFeatures = sum_features(Es, M),
    OptFeatures = sum_features(O, M),
    OptFeatures = AllFeatures,
    ok.

best_rest(Config) ->
    C = ?config(corpus, Config),
    M = ?config(element_module, Config),
    {Best, Rest} = corpus:best_rest(C),
    M:is_element(Best),
    corpus:is_corpus(Rest).

demo(Config) ->
    DataDir = ?config(data_dir, Config),
    Fn = "it_IT",
    Mod = el_string_trigram,
    Es = lists:map(fun Mod:new/1, read_file_to_strings(DataDir ++ Fn)),
    C = corpus:new(Es, Mod),
    O = corpus:optese(C),
    Ss = lists:map(fun(E) -> Mod:raw(E) end, O), 
    PrivDir = ?config(priv_dir, Config),
    write_strings_to_file(Ss, PrivDir ++ Fn ++ ".opt").

%%%% private

fixture_data(Mod, Config) ->
    [{corpus, make_corpus(Mod, Config)}, {element_module, Mod}
     | Config].

make_corpus(Mod, Config) ->
    DataDir = ?config(data_dir, Config),
    Fn = mod_to_fn(Mod),
    Fun = mod_to_fun(Mod),
    Es = lists:map(fun Mod:new/1, Fun(DataDir ++ Fn)),
    corpus:new(Es, Mod).

merge_uniq(L1, L2) ->
    lists:foldl(fun(X, Acc) ->
			case lists:member(X, Acc) of
			    true -> Acc;
			    false -> [X|Acc]
			end
		end, L2, L1).

mod_to_fn(el_bin_byte) -> "bins";
mod_to_fn(el_string_trigram) -> "it_IT_ai".
%% mod_to_fn(_) -> "bins".

mod_to_fun(el_bin_byte) -> fun read_file_to_binaries/1;
mod_to_fun(el_string_trigram) -> fun read_file_to_strings/1.
%% mod_to_fun(_) -> fun read_file_to_binaries/1.

read_file_to_binaries(Fn) ->
    {ok, File} = file:open(Fn, [read, binary]),
    read_utf8_to_bin(File).

read_file_to_strings(Fn) ->
    {ok, File} = file:open(Fn, [read, binary]),
    util:read_utf8(File).

-spec read_utf8_to_bin(term()) -> list(binary()).
read_utf8_to_bin(File) ->
    case file:read_line(File) of
        {ok, Data} ->
	    [Data | read_utf8_to_bin(File)];
	eof        -> []
    end.

sum_features(Es, Mod) ->
    lists:sort(lists:foldl(fun(E, Acc) ->
				   Fs = Mod:features(E),
				   merge_uniq(Fs, Acc)
			   end, [], Es)).

write_strings_to_file(Ss, Fn) ->
    LineSep = io_lib:nl(),
    Data = [lists:map(fun(S) -> [S, LineSep] end, Ss), LineSep],
    file:write_file(Fn, Data).
