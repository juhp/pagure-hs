# FLAG = --flag bodhi:microlens --flag bodhi:debug

stack-all:
	stack $(FLAG) --resolver nightly build
	@echo
	stack $(FLAG) --resolver lts build
	@echo
	stack $(FLAG) --resolver lts-14 build
	@echo
	stack $(FLAG) --resolver lts-13 build
	@echo
	stack $(FLAG) --resolver lts-12 build
	@echo
	stack $(FLAG) --resolver lts-11 build
	@echo
	stack $(FLAG) --resolver lts-10 build
	@echo
	stack $(FLAG) --resolver lts-9 build
	@echo
	stack $(FLAG) --resolver lts-8 build
	@echo
	stack $(FLAG) --resolver lts-6 build
