FROM ubuntu:20.04

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

RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update -y
RUN apt-get install -y \
	erlang-asn1=1:22.3.4.11-1 \
	erlang-base-hipe=1:22.3.4.11-1 \
	erlang-crypto=1:22.3.4.11-1 \
	erlang-dev=1:22.3.4.11-1 \
	erlang-eunit=1:22.3.4.11-1 \
	erlang-inets=1:22.3.4.11-1 \
	erlang-mnesia=1:22.3.4.11-1 \
	erlang-public-key=1:22.3.4.11-1 \
	erlang-runtime-tools=1:22.3.4.11-1 \
	erlang-ssl=1:22.3.4.11-1 \
	erlang-syntax-tools=1:22.3.4.11-1 \
	erlang-tools=1:22.3.4.11-1
