#!/bin/sh
mkdir -p log/sasl
erl -sname moni -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s moni -s toolbar 
