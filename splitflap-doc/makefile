SHELL = /bin/bash

scribble: splitflap.scrbl
scribble: ## Rebuild Scribble docs
	rm -rf splitflap/* || true
	scribble --htmls +m --redirect https://docs.racket-lang.org/local-redirect/ splitflap.scrbl

publish: ## Sync Scribble HTML docs to web server (doesn’t rebuild anything)
	rsync -av --delete splitflap/ $(JDCOM_SRV)what-about/splitflap/

# Self-documenting makefile (http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html)
help: ## Displays this help screen
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

.PHONY: help publish

.DEFAULT_GOAL := help
