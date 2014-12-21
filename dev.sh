#!/bin/sh
mkdir -p log
erl -sname erlything -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s erlbuild -s erlything -config erlything.config