PROJECT = erlything
DIALYZER = dialyzer
REBAR = rebar
REPO = ../../../../repository
REPOSRC = ../../repository
TARGET = ~/projects/erlang



all: app

tar: app 
	cd rel; tar cvf $(REPO)/$(PROJECT).$(VERSION).tar $(PROJECT)

tarall: app 
	cd ..; tar cf $(REPOSRC)/$(PROJECT).src.$(VERSION).tar $(PROJECT) --exclude log/* --exclude apps/horst/priv/config --exclude deps/gpio/priv/gpio_drv.so --exclude deps/syslog/priv/syslog_drv.so --exclude apps/horst/priv/config/accounts.conf --exclude Mnesia.erlything@ua-TA880GB

cpall: tarall
	cd ..;scp $(REPOSRC)/$(PROJECT).src.$(VERSION).tar $(USR)@$(HOST):$(TARGET)
	ssh $(USR)@$(HOST) 'cd $(TARGET); tar xf $(TARGET)/$(PROJECT).src.$(VERSION).tar'

cp: tar
	 cd ..;scp $(REPOSRC)/$(PROJECT).$(VERSION).tar $(USR)@$(HOST):$(TARGET)

release: app
	@$(REBAR) generate

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump
	rm -f log/*

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true


docs:
	@$(REBAR) doc skip_deps=true

rcswitch:
	$(MAKE) -C apps/horst/priv/driver/remote send

ct_setup:
	rm -rf ct_test/dev1 
	rm -rf ct_test/dev2 
	mkdir -p ct_test/dev1
	mkdir -p ct_test/dev2
	cp -R apps ct_test/dev1/apps
	cp -R deps ct_test/dev1/deps
	cp dev1.sh ct_test/dev1

	cp -R apps ct_test/dev2/apps
	cp -R deps ct_test/dev2/deps
	cp dev.sh ct_test/dev2

ct_horst:
	cd ./apps/horst
	ct_run -dir ./apps/horst/test -suite ./apps/horst/test/horst_SUITE \
	  -sname test -setcookie nocookie -logdir ./apps/horst/logs -label horst \
	  -config ./apps/horst/test/app.config -cover ./apps/horst/test/cover.spec \
	  -pa ./apps/horst/ebin
	cd ../..
