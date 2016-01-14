IMAGE_NAME ?= codeclimate/codeclimate-hlint

build:
	mkdir -p build

image:
	docker build --tag $(IMAGE_NAME)-build --file Build.dockerfile .

build/codeclimate-hlint: image build
	docker run --rm --volume "$(PWD)/build:/build" $(IMAGE_NAME)-build \
	  cp /home/app/dist/build/engine/engine /build/codeclimate-hlint

build/hlint-src: image build
	docker run --rm --volume "$(PWD)/build:/build" $(IMAGE_NAME)-build \
	  find /root/.cabal/share \
	    -type d \
	    -name 'hlint-*' \
	    -exec cp -r {} /build/hlint-src \; \
	    -quit

release: build/codeclimate-hlint build/hlint-src
	docker build --tag $(IMAGE_NAME) .

check: release
	docker run \
	  --rm --volume $(PWD):/code:ro \
	  $(IMAGE_NAME) \
	    | sed 's/\x00/,/g; s/,$$//; s/.*/[&]/' \
	    | python -mjson.tool

.PHONY: image
