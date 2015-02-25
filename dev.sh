#!/bin/sh
mkdir -p log
#erl -sname erlything -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s lager -s erlbuild -s erlything -config erlything.config
erl -sname erlything -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -mnesia dir data -s erlbuild -s erlything -config etc/app.config

