CABAL ?= cabal

SOURCE_DIRS := B/Shake/Core/ B/Shake/Core.hs

CABAL_CONFIG_FLAGS_SLOW := $(CABAL_CONFIG_FLAGS)

CABAL_CONFIG_FLAGS_FAST := \
	--disable-library-profiling \
	--disable-executable-profiling \
	$(CABAL_CONFIG_FLAGS)

CABAL_HADDOCK_FLAGS := --hyperlink-source

ifeq ($(dev), 1)
	CABAL_CONFIG_FLAGS := $(CABAL_CONFIG_FLAGS_FAST)
else
	CABAL_CONFIG_FLAGS := $(CABAL_CONFIG_FLAGS_SLOW)
endif

.NOTPARALLEL:

.PHONY: all
all: build test lint

.PHONY: clean
clean:
	$(CABAL) clean

.PHONY: deps
deps:
	$(CABAL) install $(CABAL_CONFIG_FLAGS) --only-dependencies

.PHONY: configure
configure:
	$(CABAL) configure $(CABAL_CONFIG_FLAGS)

.PHONY: doc
doc: haddock hoogle

.PHONY: haddock
haddock:
	$(CABAL) haddock $(CABAL_HADDOCK_FLAGS)

.PHONY: hoogle
hoogle:
	$(CABAL) haddock --hoogle $(CABAL_HADDOCK_FLAGS)

.PHONY: build
build:
	$(CABAL) build

.PHONY: test
test:
	$(CABAL) test

.PHONY: lint
lint: hlint

.PHONY: hlint
hlint:
	@echo Linting...
	@find $(SOURCE_DIRS) -name '*.hs' -print0 \
		| xargs -0 hlint -- >&2
