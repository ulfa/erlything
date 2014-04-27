#!/bin/sh
mkdir -p log
erl -sname erlything1 -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s lager -s horst
