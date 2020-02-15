%%%-------------------------------------------------------------------
%%% @author  <liam@lummm3>
%%% @copyright (C) 2020,
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2020 by  <liam@lummm3>
%%%-------------------------------------------------------------------
-module(token).

-behaviour(gen_server).

%% API
-export([
         start_link/2,
         to_refresh_token/1,
         to_session_token/1,
         verify_refresh_token/1,
         verify_session_token/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                priv_key,
                session_s
               }).

%%%===================================================================
%%% API
%%%===================================================================
to_refresh_token(UserId) ->
    gen_server:call(?SERVER, {to_refresh_token, UserId}).

to_session_token(UserId) ->
    gen_server:call(?SERVER, {to_session_token, UserId}).

%% return user id
verify_refresh_token(Token) ->
    gen_server:call(?SERVER, {verify_refresh_token, Token}).

%% return user id
verify_session_token(Token) ->
    gen_server:call(?SERVER, {verify_session_token, Token}).

%% from_user_id(UserId) ->
%%     gen_server:call(?SERVER, {from_user_id, UserId}).

%% to_user_id(Token) ->
%%     gen_server:call(?SERVER, {to_user_id, Token}).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(PrivKeyFile, SessionS) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [PrivKeyFile, SessionS], []).

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
init([PrivKeyFile, SessionS]) ->
    process_flag(trap_exit, true),
    {ok, PrivKey} = file:read_file(PrivKeyFile),
    _PrivKeyLines = binary:split(PrivKey, [<<"\n">>], [global]),
    {ok, #state{
            priv_key=PrivKey,
            session_s=SessionS
           }}.

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
handle_call({verify_session_token, Token}, _From,
            #state{priv_key=PrivKey} = State) ->
    {reply,
     verify_token(Token, PrivKey, false),
     State};
handle_call({verify_refresh_token, Token}, _From,
            #state{priv_key=PrivKey} = State) ->
    {reply,
     verify_token(Token, PrivKey, true),
     State};
handle_call({to_refresh_token, UserId}, _From,
            #state{priv_key=PrivKey,
                   session_s=SessionS} = State) ->
    Response = create_token(
                 UserId, PrivKey,
                 #{session_timeout_s => SessionS,
                   is_refresh => true
                  }
                ),
    {reply, Response, State};
handle_call({to_session_token, UserId}, _From,
            #state{priv_key=PrivKey,
                   session_s=SessionS} = State) ->
    Response = create_token(
                 UserId, PrivKey,
                 #{session_timeout_s => SessionS,
                   is_refresh => false
                  }
                ),
    {reply, Response, State}.

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
create_token(UserId, PrivKey,
             #{is_refresh := IsRefresh,
               session_timeout_s := SessionTimeoutS}) ->
    Claims = [
              {<<"user_id">>, UserId},
              {<<"is_refresh">>, atom_to_binary(IsRefresh, utf8)}
             ],
    jwt:encode(<<"HS256">>, Claims, SessionTimeoutS, PrivKey).

decode_token(Token, PrivKey) ->
    case jwt:decode(Token, PrivKey) of
        {ok, Claims} ->
            case Claims of
                #{
                  <<"user_id">> := UserId,
                  <<"is_refresh">> := IsRefresh
                 } -> {ok, #{
                           user_id => UserId,
                           is_refresh => binary_to_atom(IsRefresh, utf8)
                          }};
                _ -> {fail, <<"failed to parse jwt payload">>}
            end;
        JwtFail ->
            {fail, JwtFail}
    end.

verify_token(Token, PrivKey, IsRefresh) ->
    case decode_token(Token, PrivKey) of
        {ok, #{
               user_id := UserId,
               is_refresh := IsRefresh
              }} -> {ok, UserId};
        {fail, Reason} -> Reason;
        _ -> {error, bad_token_payload}
    end.
