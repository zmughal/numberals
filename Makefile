SWIPL := swipl

.PHONY: test test-numberals

test: test-numberals

test-numberals: numberals.plt
	$(SWIPL) -s $< -t 'halt' -g 'run_tests, halt; halt'
