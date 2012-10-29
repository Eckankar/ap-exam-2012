%% IVars for Erlang
%% Advanced Programming exam 2012, DIKU
%% Sebastian Paaske Tørholm <sebbe@diku.dk>

-module(pm).
-export([newVanilla/0, newPrincess/1, get/1, put/2, compromised/1]).

%% Interface

newVanilla() -> spawn(fun vanilla_loop/0).

newPrincess(Pred) -> spawn(fun() -> princess_loop(Pred) end).

get(IVar) -> rpc(IVar, get).

put(IVar, Term) -> put_async(IVar, Term).

compromised(IVar) -> rpc(IVar, compromised).

%% Asynchronous communication 
info(Pid, Msg) ->
    Pid ! Msg.

put_async(Pid, Term) ->
    info(Pid, {put, Term}).

%% Synchronous communication
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
    {Pid, Response} ->
        Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

%% Vanilla IVars
vanilla_loop() ->
    receive
        {put, Term} ->
            vanilla_loop_set(Term, false);
        {From, compromised} ->
            reply(From, false),
            vanilla_loop()
    end.

vanilla_loop_set(Val, Comp) ->
    receive
        {put, _} ->
            vanilla_loop_set(Val, true);
        {From, get} ->
            reply(From, Val),
            vanilla_loop_set(Val, Comp);
        {From, compromised} ->
            reply(From, Comp),
            vanilla_loop_set(Val, Comp)
    end.

%% Princess IVars 
princess_loop(Pred) ->
    receive
        {put, Term} ->
            try Pred(Term) of
                true -> princess_loop_set(Pred, Term);
                _    -> princess_loop(Pred)
            catch
                _    -> princess_loop(Pred)
            end;
        {From, compromised} ->
            reply(From, false),
            princess_loop(Pred)
    end.

princess_loop_set(Pred, Val) ->
    receive
        {put, _} ->
            princess_loop_set(Pred, Val);
        {From, get} ->
            reply(From, Val),
            vanilla_loop_set(Pred, Val);
        {From, compromised} ->
            reply(From, false),
            vanilla_loop_set(Pred, Val)
    end.
