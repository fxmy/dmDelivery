#!/bin/sh
erl -boot start_sasl -pa ebin deps/*/ebin -s websocket \
    -eval "io:format(\"Point your browser at http://localhost:8081/ to use a simple websocket client~n\")."

