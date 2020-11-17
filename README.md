cbeam
=====

An OTP application

Build
-----

    $ rebar3 compile

## How to Use

```shell
$ git clone https://github.com/yangcancai/cbeam.git
$ rebar3 escriptize
## decode special beamfile
$ ./cbeam _build/default/lib/cbeam/ebin/cbeam.beam > cbeam.erl
## decode all beam
$  ./cbeam _build/default/lib/cbeam/ebin erl_dir
```