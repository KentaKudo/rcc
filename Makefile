USER=$(shell id -u)
GROUP=$(shell id -g)

.PHONY: build
build:
	@docker run --rm --user $(USER):$(GROUP) -v $(PWD):/usr/src/rcc -w /usr/src/rcc rust cargo build

.PHONY: test
test: build
	@docker run --rm --user $(USER):$(GROUP) -v $(PWD):/usr/src/rcc -w /usr/src/rcc rust bash ./test.sh

.PHONY: clean
clean:
	rm tmp tmp.s
