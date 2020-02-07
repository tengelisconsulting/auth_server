-module(proxy).

-export([
         verify/2,
         proxy_to_pg/3
        ]).

verify(Req0, _State0) ->
    %% get user id, set it as a header on the req sent to postgrest
    Headers = cowboy_req:headers(Req0),
    auth_con:get_session_user_id(Headers).

proxy_to_pg(<<"GET">>, Url, UserId) ->
    Headers = #{
                <<"user-id">> => UserId
               },
    pg:get(Url, Headers).
