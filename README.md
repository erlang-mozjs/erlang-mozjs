<!--
SPDX-FileCopyrightText: 2009-2010 Basho Technologies
SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
SPDX-License-Identifier: Apache-2.0
-->

# erlang-mozjs

[![CI](https://github.com/erlang-mozjs/erlang-mozjs/actions/workflows/ci.yml/badge.svg)](https://github.com/erlang-mozjs/erlang-mozjs/actions/workflows/ci.yml)
[![REUSE status](https://api.reuse.software/badge/github.com/erlang-mozjs/erlang-mozjs)](https://api.reuse.software/info/github.com/erlang-mozjs/erlang-mozjs)

`erlang-mozjs` is a NIF library that embeds Mozilla's SpiderMonkey JavaScript
engine in Erlang. Originally created to facilitate usage of Riak's MapReduce
by non-Erlang programmers, it supports multiple concurrent JavaScript VMs,
runtime evaluation of JavaScript code, and invocation of JavaScript functions.

`erlang-mozjs` builds and executes on Unix-based platforms including Linux,
macOS, and the BSDs.

## Supported SpiderMonkey versions

The library is tested against the following mozjs versions:

| mozjs version | Fedora version |
|---------------|----------------|
| mozjs115      | Fedora 41–42   |
| mozjs128      | Fedora 43–44   |
| mozjs140      | Fedora rawhide |

The build system auto-detects the installed version via `pkg-config`.

## Requirements

- Erlang/OTP 25 or later
- rebar3
- SpiderMonkey (mozjs) development headers
- A C++17 compiler
- pkg-config

### Fedora

```sh
sudo dnf install erlang-rebar3 gcc-c++ mozjs128-devel pkg-config make
```

### Debian/Ubuntu

SpiderMonkey dev packages are available as `libmozjs-*-dev`:

```sh
sudo apt install erlang-dev g++ libmozjs-128-dev pkg-config make
```

## Quick Start

```sh
git clone https://github.com/erlang-mozjs/erlang-mozjs.git
cd erlang-mozjs
make all test
```

Start an Erlang shell:

```sh
rebar3 shell
```

Create a JavaScript VM and run some code:

```erlang
1> {ok, JS} = js_driver:new().
{ok,#Ref<0.557298074.1409679362.105432>}
2> js:define(JS, <<"var addOne = function(a){ return a + 1; }">>).
ok
3> js:call(JS, <<"addOne">>, [3]).
{ok,4}
4> js_driver:destroy(JS).
ok
```

## Building

```sh
make compile       # Compile only
make test          # Run EUnit tests
make dialyzer      # Run Dialyzer
make xref          # Run Xref
make fmt           # Format Erlang code (erlfmt)
make check-fmt     # Check formatting
make check         # All checks (test + dialyzer + xref + check-fmt)
```

## Documentation

API documentation can be generated with:

```sh
make docs
```

## License

Apache-2.0. See [LICENSE](LICENSE) for details.

## Contributing

1. Fork the repository on [GitHub](https://github.com/erlang-mozjs/erlang-mozjs).
2. Create a topic branch: `git checkout -b my-feature`
3. Make your changes and commit.
4. Push to your fork and open a pull request.

Please ensure `make check` passes before submitting.
