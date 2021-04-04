.PHONY: dev
dev:
	@docker run \
		-it \
		-v $(shell pwd -P):/workspace \
		-v $(shell readlink -f ../../):/opt/weblorg \
		-p 8000:80 \
		--rm \
		--name weblorg \
		nanzhong/weblorg
