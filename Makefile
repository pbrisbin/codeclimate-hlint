build:
	docker build --tag codeclimate/codeclimate-hlint .

check: build
	docker run \
	  --rm --volume $(PWD):/code:ro \
	  codeclimate/codeclimate-hlint \
	    | sed 's/\x00/,/g; s/,$$//; s/.*/[&]/' \
	    | python -mjson.tool

.PHONY: build check
