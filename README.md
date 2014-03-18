Deverl
======

An Erlang IDE dedicated to learning erlang, written in Erlang.
Released under the GNU general public license version 3.

### Requirements

Deverl requires a working installation of Erlang OTP which includes the wx application.
For detailed installation instructions for all platforms read [INSTALL](https://raw.github.com/deverl-ide/deverl/master/INSTALL.md).

### Starting Deverl

1. Install Erlang OTP and wxWidgets as described in Requirements.
2. Clone this repository:
         
        git clone https://github.com/deverl-ide/deverl.git
3. Change directory:

        cd deverl/ide
4. Compile:

        erl -make
5. Start Deverl, at the unix command prompt type:

        erl -pa ebin -s ide start
 or, from the Erlang shell in the root diectory of Deverl:

        ide:start().


### Folders

	/doc   - Project documentation.
	/ide   - The main project directory for source code and distribution.

### Bug/Issues

To view/report bugs viits our [trac page](http://www.tgrsvr.co.uk/trac "trac").

<div align="center"><a href="https://github.com/tomrichmond/erlangIDE"><img src="http://i.imgur.com/ckSte1h.png" alt="Erlang IDE" /></a></div>
