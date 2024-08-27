FROM erlang:slim AS builder

ARG APP=echo
ENV APP=${APP}

WORKDIR /app
COPY . .

RUN rebar3 update
RUN rebar3 compile
RUN rebar3 release as prod

FROM erlang:slim AS runner

LABEL maintainer="HUANG Cheng <cheng@wuhan.dev>"
LABEL repository="https://github.com/huangcheng/echo"
LABEL description="A simple echo server built with Erlang/OTP"
LABEL license="MIT"

ARG APP=echo
ENV APP=${APP}

WORKDIR /app
# Copy the built release for the given app name into the runtime image
COPY --from=builder /app/_build/default/rel/${APP} /app

# Copy the helper entrypoint that runs the selected release
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

# Ensure the release runner script is executable
RUN chmod +x /app/bin/${APP}

# Use the entrypoint script so $APP can be overridden at runtime
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
