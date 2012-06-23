#!/usr/bin/env sh
cd apps/bot/
rebar compile
cd ../../
erl -sname bot -pa deps/exmpp/ebin -pa apps/bot/ebin -boot start_sasl -s bot_app start
