# FLAG = --flag bodhi:microlens --flag bodhi:debug

stack-all:
	stack $(FLAG) --resolver nightly build
	@echo
	stack $(FLAG) --resolver lts-16 build
	@echo
	stack $(FLAG) --resolver lts-14 build
	@echo
	stack $(FLAG) --resolver lts-13 build
	@echo
	stack $(FLAG) --resolver lts-12 build
	@echo
	stack $(FLAG) --resolver lts-11 build
