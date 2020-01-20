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
      listen_port := ListenPort,
      db_api_port := DbApiPort,
      db_api_host := DbApiHost
     } = RuntimeArgs,
    Dispatch = cowboy_router:compile([
                                      {'_',
                                       routes()}
                                     ]),
    logger:info("starting listener on ~p", [ListenPort]),
    {ok, _} = cowboy:start_clear(auth_http_listener,
                                 [{port, ListenPort}],
                                 #{env => #{dispatch => Dispatch}}),
    auth_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
routes() ->
    [
     {"/test", base_handler,
      [base_handler, say_hi]
     },
     {"/account/create", base_handler,
      [base_handler, create_account]}
    ].
