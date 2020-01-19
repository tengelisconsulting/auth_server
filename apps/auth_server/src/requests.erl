-module(requests).

-export([
         start_db_api_con/2
        ]).


start_db_api_con(Host, Port) ->
    logger:info("opening http con at ~p:~p", [Host, Port]),
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, _Protocol} = gun:await_up(ConnPid).
