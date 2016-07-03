-module(element_behaviour).
-include("types.hrl").

-export([behaviour_info/1
	]).

behaviour_info(callbacks) ->
    [{new, 1}
    ,{is_element, 1}
    ,{features, 1}
    ,{raw, 1}
    ,{score, 1}
    ,{update, 2}
    ];
behaviour_info(_) ->
    undefined.
