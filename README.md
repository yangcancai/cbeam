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
##$ which erl
## like: /Users/admin/.erlangInstaller/22.3.4.1/bin/erl
## cp cbeam  /Users/admin/.erlangInstaller/22.3.4.1/bin/
$ cp cbeam erldir
## decode special beamfile
$ cbeam _build/default/lib/cbeam/ebin/cbeam.beam > cbeam.erl
## decode all beam
$  cbeam _build/default/lib/cbeam/ebin erl_dir
```