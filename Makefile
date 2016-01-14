IMAGE_NAME ?= codeclimate/codeclimate-hlint

build:
	mkdir -p build
	docker build --tag codeclimate/codeclimate-hlint-build --file Build.dockerfile .
	docker run --rm --volume "$(PWD)/build:/build" codeclimate/codeclimate-hlint-build cp /home/app/dist/build/engine/engine /build/codeclimate-hlint
	docker run --rm --volume "$(PWD)/build:/build" codeclimate/codeclimate-hlint-build cp -r /root/.cabal/share/x86_64-linux-ghc-7.10.2/hlint-1.9.26/ /build/hlint-src
	docker build --tag $(IMAGE_NAME) .

check: build
	docker run \
	  --rm --volume $(PWD):/code:ro \
	  $(IMAGE_NAME) \
	    | sed 's/\x00/,/g; s/,$$//; s/.*/[&]/' \
	    | python -mjson.tool

.PHONY: build check
