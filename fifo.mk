REBAR = $(shell pwd)/rebar3

compile: $(REBAR) .git/hooks/pre-commit
	$(REBAR) compile

.git/hooks/pre-commit: hooks/pre-commit
	cp hooks/pre-commit .git/hooks

pre-commit: test-scripts lint xref dialyzer test

dialyzer: $(REBAR)
	$(REBAR) dialyzer

xref: $(REBAR)
	$(REBAR) xref

test-scripts:
	for i in rel/files/*; do (head -1 $$i | grep -v sh > /dev/null) || bash -n $$i || exit 1; done;

test: $(REBAR)
	$(REBAR) eunit

lint: $(REBAR)
	$(REBAR) as lint lint

$(REBAR):
	cp `which rebar3` $(REBAR)

upgrade: $(REBAR)
	$(REBAR) upgrade 
	make tree

update: $(REBAR)
	$(REBAR) update

tree: $(REBAR)
	$(REBAR) tree | grep -v '=' | sed 's/ (.*//' > tree

tree-diff: tree
	git diff test -- tree

###
### Docs
###
docs:
	$(REBAR) edoc

