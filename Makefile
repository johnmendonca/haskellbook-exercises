package = haskellbook-exercises

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build $(package)

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

run:
	$(stack) build --fast && $(stack) exec -- $(package)-exe

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib --ghci-options='-j4'

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-test --ghci-options='-j4'

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid --allow-eval -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind -j4' --main-is $(package):$(package)-exe"

.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps
