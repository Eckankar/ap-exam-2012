%% Unit tests for pm module 
%% Advanced Programming exam 2012, DIKU
%% Sebastian Paaske Tørholm <sebbe@diku.dk>

-module(pmtest).
-include_lib("eunit/include/eunit.hrl").

%% Vanilla IVars {{{ 
% Putting and getting gives the same element back
vanilla_put_get_test() ->
    IV = pm:newVanilla(),
    pm:put(IV, 5),
    ?assert(5 =:= pm:get(IV)).

get_block_getter() ->
    receive
        {From, {get, IV}} ->
            V = pm:get(IV),
            From ! {got, V}
    end.

% Getting blocks until data is put into IVar
vanilla_get_block_test() ->
    IV = pm:newVanilla(),
    P = spawn(fun() -> get_block_getter() end),
    P ! {self(), {get, IV}},

    timer:sleep(453),

    pm:put(IV, data_packet),
    receive
        {got, data_packet} -> ?assert(true);
        _                  -> ?assert(false)
    end.

% compromised acts according to spec
vanilla_compromised_test() ->
    IV = pm:newVanilla(),
    ?assert(not(pm:compromised(IV))),
    pm:put(IV, some_data),
    ?assert(not(pm:compromised(IV))),
    pm:put(IV, some_more_data),
    ?assert(pm:compromised(IV)).

% cannot overwrite value
vanilla_immutable_test() ->
    IV = pm:newVanilla(),
    pm:put(IV, something),
    pm:put(IV, something_else),
    ?assert(something =:= pm:get(IV)).
%% }}}
%% Princess IVars {{{

accept(_) -> true.

% Putting and getting gives the same element back
princess_put_get_test() ->
    IV = pm:newPrincess(fun accept/1),
    pm:put(IV, 5),
    ?assert(5 =:= pm:get(IV)).

% Getting blocks until data is put into IVar
princess_get_block_test() ->
    IV = pm:newPrincess(fun accept/1),
    P = spawn(fun() -> get_block_getter() end),
    P ! {self(), {get, IV}},

    timer:sleep(453),

    pm:put(IV, data_packet),
    receive
        {got, data_packet} -> ?assert(true);
        _                  -> ?assert(false)
    end.

% compromised acts according to spec
princess_compromised_test() ->
    IV = pm:newPrincess(fun accept/1),
    ?assert(not(pm:compromised(IV))),
    pm:put(IV, some_data),
    ?assert(not(pm:compromised(IV))),
    pm:put(IV, some_more_data),
    ?assert(not(pm:compromised(IV))).

% cannot overwrite value
princess_immutable_test() ->
    IV = pm:newPrincess(fun accept/1),
    pm:put(IV, something),
    pm:put(IV, something_else),
    ?assert(something =:= pm:get(IV)).

% Getting doesn't get anything until predicate is satisfied
princess_get_block_pred_test() ->
    IV = pm:newPrincess(fun(V) -> V > 5 end),
    P = spawn(fun() -> get_block_getter() end),
    P ! {self(), {get, IV}},

    pm:put(IV, 4),
    timer:sleep(123),
    pm:put(IV, 2),
    timer:sleep(234),
    pm:put(IV, 23),
    timer:sleep(113),
    pm:put(IV, 43),

    receive
        {got, 23} -> ?assert(true);
        _         -> ?assert(false)
    end,
    ?assert(23 =:= pm:get(IV)).

% Princess predicates handles non-boolean return values and exceptions correctly
bad_predicate(1) -> 5;
bad_predicate(2) -> throw(true);
bad_predicate(3) -> erlang:error(wrong_stuff);
bad_predicate(4) -> exit(stuff);
bad_predicate(_) -> true.

princess_bad_pred_test() ->
    IV = pm:newPrincess(fun bad_predicate/1),
    P = spawn(fun() -> get_block_getter() end),
    P ! {self(), {get, IV}},

    pm:put(IV, 1),
    pm:put(IV, 2),
    pm:put(IV, 3),
    pm:put(IV, 4),
    pm:put(IV, 5),

    receive
        {got, 5} -> ?assert(true);
        _        -> ?assert(false)
    end.

%% }}}
