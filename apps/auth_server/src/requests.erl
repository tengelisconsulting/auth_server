%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 19 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(requests).

-behaviour(gen_server).

%% API
-export([
         start_link/2,
         get/2,
         post/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                con_pid=undefined,
                protocol=undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================
get(ReqPs, Url) ->
    gen_server:call(ReqPs, {get, Url}).

post(ReqPs, Url, Data) ->
    gen_server:call(ReqPs, {post, Url, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(ServerId, [Host, Port]) ->
    gen_server:start_link({local, ServerId}, ?MODULE, [ServerId, [Host, Port]], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([ServerId, [Host, Port]]) ->
    process_flag(trap_exit, true),
    logger:info("req server ~p opening con at ~s:~p", [ServerId, Host, Port]),
    case gun:open(Host, Port) of
        {ok, ConnPid} ->
            {ok, Protocol} = gun:await_up(ConnPid),
            {ok, #state{con_pid=ConnPid, protocol=Protocol}};
        {error, _} ->
            logger:error("failed to open con ~s:~p", [Host, Port]),
            {stop, error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({post, Url, Data}, _From, State) ->
    logger:info("handling post to ~p", [Url]),
    #state{con_pid=ConPid}=State,
    ReqHeaders = [],
    StreamRef = gun:post(ConPid, Url, ReqHeaders, Data),
    case gun:await(ConPid, StreamRef) of
        {response, fin, Status, _Headers} ->
            {reply, {Status, no_data}, State};
        {response, nofin, Status, _Headers} ->
            {ok, Body} = gun:await_body(ConPid, StreamRef),
            {reply, {Status, Body}, State}
    end;
handle_call({get, Url}, _From, State) ->
    logger:info("handling get to ~p", [Url]),
    #state{con_pid=ConPid}=State,
    StreamRef = gun:get(ConPid, Url),
    case gun:await(ConPid, StreamRef) of
        {response, fin, Status, _Headers} ->
            {reply, {Status, no_data}, State};
        {response, nofin, Status, _Headers} ->
            {ok, Body} = gun:await_body(ConPid, StreamRef),
            {reply, {Status, Body}, State}
    end.
%% handle_call(_Request, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
