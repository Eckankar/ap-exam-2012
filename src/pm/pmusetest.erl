%% Unit tests for pmuse module 
%% Advanced Programming exam 2012, DIKU
%% Sebastian Paaske Tørholm <sebbe@diku.dk>

-module(pmusetest).
-include_lib("eunit/include/eunit.hrl").

%% pmmap {{{
% Compare pmmap to lists:map
pmmap_0_test() ->
    F = fun(E) -> E*E end,
    L = lists:seq(1, 1000),
    ?assert( lists:map(F, L) =:= pmuse:pmmap(F, L) ).

pmmap_1_test() ->
    F = fun(E) -> E+5 end,
    L = [],
    ?assert( lists:map(F, L) =:= pmuse:pmmap(F, L) ).

pmmap_2_test() ->
    F = fun(E) -> {ok, E} end,
    L = [65, 3, 7, 3, 87, 32, 5, 2, 6],
    ?assert( lists:map(F, L) =:= pmuse:pmmap(F, L) ).
%% }}}
%% treeforall {{{
% true on empty tree
reject(_) -> false.

tfa_accept_empty_test() ->
    ?assert(pmuse:treeforall(leaf, fun reject/1)).

% treeforall doesn't evaluate 
% this test uses that the tree is evaluated in preorder.
retimm_pred(3) -> retimm_pred(3);
retimm_pred(1) -> false;
retimm_pred(_) -> true.

tfa_returns_immediately_test() ->
    Tree = {node, 2, {node, 1, leaf, leaf},
                     {node, 3, leaf, leaf}},
    ?assert(not(pmuse:treeforall(Tree, fun retimm_pred/1))).

% treeforall on some regular trees
tfa_0_test() ->
    Tree = {node, 5, {node, 3, {node, 2, {node, 1, leaf, leaf}, leaf},
                               {node, 4, leaf, leaf}},
                     {node, 8, leaf, leaf}},
    Pred = fun(V) -> V < 10 end,
    ?assert(pmuse:treeforall(Tree, Pred)).

tfa_1_test() ->
    Tree = {node, 5, {node, 3, {node, 2, {node, 1, leaf, leaf}, leaf},
                               {node, 4, leaf, leaf}},
                     {node, 8, leaf, leaf}},
    Pred = fun(V) -> V < 8 end,
    ?assert(not(pmuse:treeforall(Tree, Pred))).
%% }}}
