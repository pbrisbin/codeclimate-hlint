FROM alpine:3.3
MAINTAINER Pat Bribin <pat@codeclimate.com>

RUN adduser -u 9000 -h /home/app -D app
USER app
WORKDIR /home/app

COPY build/ /home/app/
COPY LICENSE /home/app/LICENSE

VOLUME /code
WORKDIR /code

ENV HLINT_DATA_DIR /home/app/hlint-src
ENTRYPOINT ["/home/app/codeclimate-hlint"]
