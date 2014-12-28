#!/bin/sh
mkdir -p log
<<<<<<< HEAD
erl -sname erlything -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s lager -s erlbuild -s erlything -config erlything.config
=======
erl -sname erlything -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s erlbuild -s erlything -config erlything.config
>>>>>>> 351af1db24dfc9ec1fd747113110c60fcb992c58
