# Checking for required tools.
ifeq (,$(shell command -v dune 2> /dev/null))
$(error "Compilation requires [dune].")
endif

# GNU vs BSD
ifeq (GNU,$(shell sed --version 2>&1 > /dev/null && echo GNU))
SEDI = sed -i
else
SEDI = sed -i ''
endif

PROFILE := dev
# Trick to avoid printing the commands.
# To enable the printing of commands, use [make Q= ...],
Q = @

.PHONY: default
default: cn

.PHONY: all
all: cn cn-coq

.PHONY: full-build
full-build:
	@echo "[DUNE] full-build"
	$(Q)dune build --profile=$(PROFILE)

.PHONY: cn
cn:
	@echo "[DUNE] $@"
	$(Q)dune build -p cn --profile=$(PROFILE)

.PHONY: clean
clean:
	$(Q)rm -f cn-coq.install cn.install
	$(Q)rm -f coq/*.{glob,vo,vok}
	$(Q)rm -rf _build/

.PHONY: install
install: cn
	@echo "[DUNE] install cn"
	$(Q)dune install cn --profile=$(PROFILE)

.PHONY: uninstall
uninstall:
	@echo "[DUNE] uninstall cn"
	$(Q)dune uninstall cn

.PHONY: cn-coq
cn-coq:
	@echo "[DUNE] cn-coq"
	$(Q)dune build -p cn-coq --profile=$(PROFILE)

.PHONY: cn-coq-install
cn-coq-install: cn-coq
	@echo "[DUNE] install cn-coq"
	$(Q)dune install cn-coq --profile=$(PROFILE)

.PHONY: cn-with-coq
cn-with-coq:
	@echo "[DUNE] cn,cn-coq"
	$(Q)dune build -p cn,cn-coq --profile=$(PROFILE)

# Development target to watch for changes in cn/lib and rebuild
# e.g. to be used with vscode IDE
.PHONY: cn-dev-watch
cn-dev-watch:
	@echo "[DUNE] cn-dev-watch"
	$(Q)dune build --watch -p cn,cn-coq --profile=$(PROFILE)
