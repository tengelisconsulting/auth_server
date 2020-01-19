FROM tengelisconsulting/erlang_build:22.2.2-alpine

WORKDIR /build

COPY rebar.config rebar.lock /build/
RUN rebar3 compile

COPY config /build/config
COPY apps /build/apps
RUN rebar3 as prod tar


FROM erlang:22.2.2-alpine
WORKDIR /app
COPY --from=0 /build/_build/prod/rel/auth_server/auth_server-0.1.0.tar.gz .
RUN tar xzf ./auth_server-0.1.0.tar.gz \
        && rm ./auth_server-0.1.0.tar.gz

ENTRYPOINT [ "/app/bin/auth_server", "foreground" ]
