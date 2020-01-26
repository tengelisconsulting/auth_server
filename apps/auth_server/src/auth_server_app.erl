%%%-------------------------------------------------------------------
%% @doc auth_server public API
%% @end
%%%-------------------------------------------------------------------

-module(auth_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, RuntimeArgs} = application:get_env(auth_server, runtime_args),
    #{
      listen_port := ListenPortStr
     } = RuntimeArgs,
    Dispatch = cowboy_router:compile([
                                      {'_',
                                       routes()}
                                     ]),
    logger:info("starting listener on ~p", [ListenPortStr]),
    {ok, _} = cowboy:start_clear(auth_http_listener,
                                 [{port, list_to_integer(ListenPortStr)}],
                                 #{env => #{dispatch => Dispatch}}),
    auth_server_sup:start_link(RuntimeArgs).

stop(_State) ->
    ok.

%% internal functions
routes() ->
    [
     {"/permission/check", base_handler,
      [base_handler, check_permission]},
     {"/authenticate/username-password", base_handler,
      [base_handler, auth_username_password]},
     {"/account/create", base_handler,
      [base_handler, create_account]},
     {"/session/user-id", base_handler,
      [base_handler, user_id]}
    ].
