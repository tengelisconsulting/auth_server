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
create_account(allowed_methods) -> [<<"PUT">>].
create_account(Req0, State0) ->
    Success = true,
    Response = <<"creating user...">>,
    {Success, Response, Req0, State0}.

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
    case jsone:decode(AuthResponse) of
        <<"">> ->
            {halt, <<"">>, cowboy_req:reply(401, Req0), State0};
        UserId ->
            {ok, Token} = token:from_user_id(UserId),
            Req = cowboy_req:set_resp_cookie(
                     <<"auth_token">>, Token, Req0
                   ),
            {true, Token, Req, State0}
    end.

%% Internal
get_user_id(Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<"">>),
    case binary:split(AuthHeader, <<"Bearer ">>) of
        [<<>>, AuthToken] ->
            token:to_user_id(AuthToken);
        _ -> {error, no_auth_header}
    end.
