FROM ubuntu:20.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -y update

RUN  apt-get install -y  \
	build-essential \
	debhelper \
	devscripts \
	fakeroot \
	gcc \
	git \
	libmozjs-68-dev \
	make \
	pkg-config \
	wget

RUN wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
RUN apt-key add erlang_solutions.asc
RUN echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" > /etc/apt/sources.list.d/esl-erlang.list
RUN apt-get update -y
RUN apt-get install -y \
	erlang-asn1=1:23.1-1 \
	erlang-base-hipe=1:23.1-1 \
	erlang-crypto=1:23.1-1 \
	erlang-dev=1:23.1-1 \
	erlang-eunit=1:23.1-1 \
	erlang-inets=1:23.1-1 \
	erlang-mnesia=1:23.1-1 \
	erlang-public-key=1:23.1-1 \
	erlang-runtime-tools=1:23.1-1 \
	erlang-ssl=1:23.1-1 \
	erlang-syntax-tools=1:23.1-1 \
	erlang-tools=1:23.1-1

RUN git clone https://github.com/rebar/rebar.git
RUN cd rebar && ./bootstrap
RUN install -m 755 -p ./rebar/rebar /usr/bin/rebar
