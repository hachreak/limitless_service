.PHONY: clean all run

node1: compile run1

node2: compile run2

clean:
	rm data_* data.* log* ring_state_* ttb_last_config compile_commands.json undefined -Rf

compile:
	rebar3 compile

run1:
	rebar3 shell --name test1@127.0.0.1 --config priv/limitless_service_node1.config --apps limitless_service --user_drv_args priv/vm.args

run2:
	rebar3 shell --name test2@127.0.0.1 --config priv/limitless_service_node2.config --apps limitless_service --user_drv_args priv/vm.args
