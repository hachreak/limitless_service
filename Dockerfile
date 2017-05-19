FROM hachreak/erlang:stretch-erlang19

COPY _build/default/rel/limitless_service /src/code
USER root
RUN chown erlang:erlang /src/code  -R
USER erlang
