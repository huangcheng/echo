#!/bin/sh
# docker-entrypoint.sh
# Runs the release for the app specified by $APP.
# If APP isn't set at runtime, it falls back to the value baked into the image.

# Default APP if none provided (this mirrors the Dockerfile's ARG default)
: "${APP:=echo}"

# Execute the release in foreground so Docker can manage the process.
exec "/app/bin/${APP}" foreground

