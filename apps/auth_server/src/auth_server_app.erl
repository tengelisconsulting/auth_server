%%%-------------------------------------------------------------------
%% @doc auth_server public API
%% @end
%%%-------------------------------------------------------------------

-module(auth_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, #{
           server_type:=ServerStr
          }} = application:get_env(auth_server, server_type),
    Server = list_to_atom(ServerStr),
    {ok, #{
           Server := RuntimeArgs
          }} = application:get_env(auth_server, runtime_args),
    #{
      listen_port := ListenPortStr
     } = RuntimeArgs,
    Dispatch = cowboy_router:compile([
                                      {'_',
                                       routes(Server)}
                                     ]),
    logger:info("starting listener for ~p on ~p", [Server, ListenPortStr]),
    {ok, _} = cowboy:start_clear(auth_http_listener,
                                 [{port, list_to_integer(ListenPortStr)}],
                                 #{env => #{dispatch => Dispatch}}),
    SupMod = get_supervisor(Server),
    SupMod:start_link(RuntimeArgs).

stop(_State) ->
    ok.

%% internal functions
routes(auth_server) ->
    [
     {"/permission/check", base_handler,
      [base_handler, check_permission]},
     {"/authenticate/username-password", base_handler,
      [base_handler, auth_username_password]},
     {"/account/create", base_handler,
      [base_handler, create_account]},
     {"/session/user-id", base_handler,
      [base_handler, user_id]}
    ];
routes(api_server) ->
    [
     {"/api/[...]", base_handler,
      [base_handler, api_request]}
    ].

get_supervisor(auth_server) ->
    auth_server_sup;
get_supervisor(api_server) ->
    api_server_sup.
