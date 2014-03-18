##Erlang/wxWidgets Installation


###MacOSX 10.9

**Building wxWidgets**

Download/extract wxWidgets-3.x, then:
<pre><code>cd wxWidgets-3.x
mkdir build-rel
cd build-rel
../configure --with-cocoa --prefix="$(pwd)" --enable-debug
make
PATH="$(pwd)":$PATH ; export PATH
</code></pre>

**Building erlang**

Download Erlang OTP (minimum version R17rc2)
<pre><code>cd otp_src_R17XX
otpbuild autoconf (only required when downloading non-release sources)
./configure --enable-darwin-64bit
make
make install (optional, will install to default directory [usually /usr/local])
</code></pre>

**R16 Releases**

A bug in the Erlang wx-binding causes the Erlang Runtime System to crash.

If you're using R16 then you will need to perform the following fix:
Delete the line:

**Disconnect andalso (catch wxEvtHandler:disconnect_impl(CbH,O)),**

in wxe_server (otp_src_R16XX/lib/wx/src).

**Pre-built Binaries**

Previous pre-built binaries are available from [Erlang Solutions](https://www.erlang-solutions.com/downloads/), but currently they include an old version of wxWidgets (2.8) in the 32-bit version, and disable wx completely in the 64-bit version. 


###LINUX

**Ubuntu Trusty**

Open terminal, type:

`sudo apt-get install build-essential libwxgtk3.0-dev freeglut3-dev libncurses5-dev`

to install necessary packages, then download and extract the latest erlang release from [here](http://www.erlang.org/download.html here).

Now type:
<pre><code>cd otp_src_xxx
./configure
make
</code></pre>

**Ubuntu 13**

Open terminal, type:

`sudo apt-get install build-essential freeglut3-dev libncurses5-dev libgtk-3-dev`

Download/extract wxWidgets-3.x, then:
<pre><code>cd wxWidgets-3.x
mkdir build-rel
cd build-rel
../configure --prefix="$(pwd)" --enable-debug
make
PATH="$(pwd)":$PATH ; export PATH
</code></pre>

to install necessary packages, then download and extract the latest erlang release from [here](http://www.erlang.org/download.html here).

Now type:

<pre><code>cd otp_src_xxx
./configure
make
</pre></code>


### Microsoft Windows

The easiest method is to download and install a pre-build binary from [Erlang Solutions](https://www.erlang-solutions.com/downloads/). Note that these packages might not contain the latest version of wxWidgets.


### Finally

**Test the installation**
<pre><code>cd bin
./erl -smp</pre></code>

In the erlang shell type:
`wx:demo().`
