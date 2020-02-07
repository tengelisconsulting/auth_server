-module(base_handler).

%% cowboy
-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         json_to_input/2,
         res_to_json/2
        ]).

%% handler fns
-export([
         api_request/1, api_request/2,
         user_id/1, user_id/2,
         check_permission/1, check_permission/2,
         create_account/1, create_account/2,
         auth_username_password/1, auth_username_password/2
        ]).


-record(state, {op=[nomod, nofun],
                data=none}).

%% cowboy
init(Req, [OpMod, OpFn]) ->
    State = #state{op=[OpMod, OpFn]},
    {cowboy_rest, Req, State}.

allowed_methods(Req, #state{op=[Mod, Fn]} = State) ->
    Methods = erlang:apply(Mod, Fn, [allowed_methods]),
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, json_to_input},
      {<<"*/*">>, json_to_input},
      {undefined, json_to_input}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, res_to_json},
      {undefined, res_to_json}
     ], Req, State}.

json_to_input(Req0, State0) ->
    {ok, DataStr, Req1} = read_body(Req0),
    Data = jsone:decode(DataStr),
    State1 = State0#state{data=Data},
    {Success, Response, Req2, State} = handle(Req1, State1),
    BodyAsJson = jsone:encode(Response),
    Req = cowboy_req:set_resp_body(BodyAsJson, Req2),
    {Success, Req, State}.

res_to_json(Req0, State0) ->
    {_Success, Response, Req, State} = handle(Req0, State0),
    BodyAsJson = jsone:encode(Response),
    {BodyAsJson, Req, State}.


%% internal
read_body(Req0) ->
    cowboy_req:read_body(Req0).

handle(Req0, #state{op=[Mod, Fn]} = State0) ->
    erlang:apply(Mod, Fn, [Req0, State0]).


%% API
create_account(allowed_methods) -> [<<"POST">>].
create_account(Req0, #state{data=Data}=State0) ->
    #{
      <<"username">> := Username,
      <<"password">> := Password
     } = Data,
    case pg:init_user(Username, Password) of
        {200, UserId} ->
            {true, UserId, Req0, State0};
        {Status, Details} ->
            Req1 = cowboy_req:set_resp_body(
                     jsone:encode(Details), Req0
                    ),
            {halt, Details,
             cowboy_req:reply(Status, Req1),
             State0}
    end.

check_permission(allowed_methods) -> [<<"GET">>].
check_permission(Req0, State0) ->
    #{code := PermissionCode} = cowboy_req:match_qs([code], Req0),
    case get_user_id(Req0) of
        {ok, UserId} ->
            Response = mem_db:check_permission(UserId, PermissionCode),
            {true, Response, Req0, State0};
        _ ->
            {halt, <<"unauthorized">>,
             cowboy_req:reply(401, Req0), State0}
    end.

auth_username_password(allowed_methods) -> [<<"POST">>].
auth_username_password(Req0, #state{data=Data} = State0) ->
    #{
      <<"username">> := Username,
      <<"password">> := Password
     } = Data,
    {200, AuthResponse} = pg:check_username_password(
                 binary_to_list(Username),
                 binary_to_list(Password)
                ),
    case AuthResponse of
        <<"">> ->
            {halt, <<"">>, cowboy_req:reply(401, Req0), State0};
        UserId ->
            {ok, Token} = token:from_user_id(UserId),
            Req = cowboy_req:set_resp_cookie(
                     <<"auth_token">>, Token, Req0
                   ),
            {true, Token, Req, State0}
    end.

user_id(allowed_methods) -> [<<"GET">>].
user_id(Req0, State0) ->
    case get_user_id(Req0) of
        {ok, UserId} ->
            {true, UserId, Req0, State0};
        _ ->
            {false, <<"">>,
             cowboy_req:reply(401, Req0), State0}
    end.

api_request(allowed_methods) -> [<<"GET">>, <<"POST">>, <<"PUT">>].
api_request(Req0, State0) ->
    {Status, UserId} = proxy:verify(Req0, State0),
    case Status of
        200 ->
            FullPath = cowboy_req:path(Req0),
            [<<>>, EffectivePath] = binary:split(FullPath, <<"/api">>),
            Qs = cowboy_req:qs(Req0),
            {Status, Response} = proxy:proxy_to_pg(
                                   cowboy_req:method(Req0),
                                   <<EffectivePath/binary, <<"?">>/binary, Qs/binary>>,
                                   UserId
                                  ),
            %% %% Req = cowboy_req:set_resp_body(Response),
            %% Req = cowboy_req:reply(Status, Req0),
            {true, Response, Req0, State0};
        _ ->
            {false, <<"">>, cowboy_req:reply(401, Req0), State0}
    end.


%% Internal
get_user_id(Req) ->
    %% you need to reorganize this, because it is tempting to call this
    %% function from the api_server, but 'token' will not be running there
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<"">>),
    case binary:split(AuthHeader, <<"Bearer ">>) of
        [<<>>, AuthToken] ->
            token:to_user_id(AuthToken);
        _ -> {error, no_auth_header}
    end.
