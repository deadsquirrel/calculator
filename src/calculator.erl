%%%-------------------------------------------------------------------
%%% @author Yana P. Ribalchenko <yanki@hole.lake>
%%% @copyright (C) 2015, Yana P. Ribalchenko
%%% @doc
%%%       My  gen_server calculator
%%% @end
%%% Created :  9 Nov 2015 by Yana P. Ribalchenko <yanki@hole.lake>
%%%-------------------------------------------------------------------
-module(calculator).

-behaviour(gen_server).

-define(VERSION, 0.01).
-define (TIMEOUT, 5000).
-define (PORT, 7).

%% API
-export([
         start_link/0,
         get_state/0,
         set_internal_var/1,
         set_fun_math/1,
         set_operand_one/1,
         set_operand_two/1,
         set_operands/2,
         perform_operation/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          time_started :: calendar:datetime(),
          req_processed = 0 :: integer(),
          internal_var :: any(),
          fun_math :: fun (),
          operand_one :: integer (),
          operand_two :: integer ()
        }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    io:format("calc gen_server start_link (pid ~p)~n", [self()]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc just a demo of a API call
%% @end
%%--------------------------------------------------------------------
-spec get_state() -> #state{}.
get_state() ->
    io:format("get_state/0, pid: ~p~n", [self()]),
    gen_server:call(?SERVER, get_me_state).

perform_operation() ->
    io:format("perform_operation/0, pid: ~p~n", [self()]),
    gen_server:call(?SERVER, get_me_result).

%%--------------------------------------------------------------------
%% @doc just a demo of a API call
%%  устанавливаем значения элементов в state
%% @end
%%--------------------------------------------------------------------
-spec set_internal_var(NewVal :: any()) -> ok.
set_internal_var(NewVal) ->
    io:format("set_internal_var/1, pid: ~p~n", [self()]),
    gen_server:call(?SERVER, {set_int_var, NewVal}).

-spec set_fun_math(Function :: fun()) -> ok.
set_fun_math(Function) ->
    io:format("set_fun_math/1, pid: ~p~n", [self()]),
    gen_server:call(?SERVER, {fun_yanki, Function}).

-spec set_operand_one(A :: integer()) -> ok.
set_operand_one(A) ->
    io:format("set_operand_one/1, pid: ~p, ~p~n", [self(), A]),
    gen_server:call(?SERVER, {opA, A}).

-spec set_operand_two(B :: integer()) -> ok.
set_operand_two (B) ->
    io:format("set_operand_two/1, pid: ~p, ~p~n", [self(), B]),
    gen_server:call(?SERVER, {opB, B}).

-spec set_operands(Ds1 :: integer(), Ds2 :: integer()) -> ok.
set_operands (Ds1, Ds2) ->
    io:format("set_operands/2, pid: ~p, ~p, ~p~n", [self(), Ds1, Ds2]),
    gen_server:call(?SERVER, {operands, Ds1, Ds2}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("calc gen_server init fun (pid ~p)~n", [self()]),
    TS = erlang:localtime(),
    {ok, #state{time_started = TS}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_me_state, _From, State) ->
    io:format("handle_call/3 (get_me_state), pid: ~p~n", [self()]),
    CurrNum = State#state.req_processed,
    {reply, {takeit, State}, State#state{req_processed = CurrNum +1}};

handle_call({set_int_var, IntVar}, _From, State) ->
    io:format("handle_call/3 (set IntVar), pid: ~p~n", [self()]),
    {reply, ok_blia, State#state{internal_var = IntVar}};

handle_call({opA, SetA}, _From, State) ->
    io:format("handle_call/3 (set A), pid: ~p~n", [self()]),
    {reply, ok_A, State#state{operand_one = SetA}};

handle_call({opB, SetB}, _From, State) ->
    io:format("handle_call/3 (set B), pid: ~p~n", [self()]),
    {reply, ok_B, State#state{operand_two = SetB}};

handle_call({fun_yanki, Function}, _From, State) ->
    io:format("handle_call/3 (fun_math), pid: ~p~n", [self()]),
    {reply, ok_fun, State#state{fun_math = Function}};

handle_call({operands, Op1, Op2}, _From, State) ->
    io:format("handle_call/3 (operands), pid: ~p~n", [self()]),
    {reply, okey, State#state{operand_one = Op1, operand_two = Op2}};

handle_call(get_me_result, _From, State) ->
    io:format("handle_call/3 (get_me_result), pid: ~p~n", [self()]),
    F = State#state.fun_math,
    A = State#state.operand_one,
    B = State#state.operand_two,
    {reply,  F ( A, B ),  State};

handle_call(_Request, _From, State) ->
    io:format("handle_call/3 (default) , pid: ~p~n", [self()]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    io:format("handle_cast/2 (default) , pid: ~p~n", [self()]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    io:format("handle_info/2 (default) , pid: ~p~n", [self()]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("terminate/2, pid: ~p~n", [self()]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    io:format("code_change/3, pid: ~p~n", [self()]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

