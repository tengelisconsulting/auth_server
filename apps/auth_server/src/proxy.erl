-module(proxy).


verify(Req0, State0) ->
    %% get user id, set it as a header on the req sent to postgrest
    Headers = cowboy_req:headers(Req0).
    
