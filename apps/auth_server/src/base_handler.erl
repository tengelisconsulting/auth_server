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
         say_hi/1, say_hi/2,
         create_account/1, create_account/2
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
say_hi(allowed_methods) -> [<<"GET">>].
say_hi(Req0, State0) ->
    Success = true,
    Response = <<"Hello get">>,
    {Success, Response, Req0, State0}.

create_account(allowed_methods) -> [<<"PUT">>].
create_account(Req0, State0) ->
    Success = true,
    Response = <<"creating user...">>,
    {Success, Response, Req0, State0}.
