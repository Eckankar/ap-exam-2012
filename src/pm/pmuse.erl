%% Utility functions working with IVars
%% Advanced Programming exam 2012, DIKU
%% Sebastian Paaske Tørholm <sebbe@diku.dk>

-module(pmuse).
-export([pmmap/2, treeforall/2]).

%% Interface

pmmap(Fun, List) ->
    Mappers = init_mappers(20, fun (D, IV) -> pm:put(IV, Fun(D)) end),
    Data    = lists:map(fun(D) -> {D, pm:newVanilla()} end, List),
    send_data(Mappers, Data),
    Results = lists:map(fun({_,IV}) -> pm:get(IV) end, Data),
    lists:foreach(fun stop_async/1, Mappers),

    Results.

treeforall(Tree, Pred) ->
    Mappers = init_mappers(20, fun(D, IV) ->
        pm:put(IV, {ok, D}),
        pm:put(IV, notok)
    end),
    List    = preorder_treewalk(Tree),
    OurPred = fun({ok, D}) -> Pred(D);
                 (notok)   -> true
              end,
    Data    = lists:map(fun(D) -> {D, pm:newPrincess(OurPred)} end, List),
    send_data(Mappers, Data),
    Result = try treeforall_reduce(Data) of
                _ -> true
             catch
                throw:false -> false
             end,
    lists:foreach(fun stop_async/1, Mappers),

    Result.

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

data_async(Pid, D) ->
    info(Pid, {data, D}).

stop_async(Pid) ->
    info(Pid, stop).

%% Implementation

init_mappers(0, _) -> [];
init_mappers(N, Fun) ->
    Mapper = spawn(fun() -> mapper_loop(Fun) end),
    [Mapper | init_mappers(N-1, Fun)].

send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).

preorder_treewalk(leaf) -> [];
preorder_treewalk({node, E, L, R}) ->
    [E | preorder_treewalk(L) ++ preorder_treewalk(R)].

mapper_loop(Fun) ->
    receive
        {data, {Data, IVar}} ->
            Fun(Data, IVar),
            mapper_loop(Fun);
        stop ->
            ok
    end.

treeforall_reduce(Data) ->
    lists:foreach(fun({_,IV}) ->
        case pm:get(IV) of
            {ok, _} -> true;
            notok   -> throw(false)
        end
    end, Data).
