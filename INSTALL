Erlang/wxWidgets Installation


MacOSX 10.9

## Build wxWidgets

Download/extract wxWidgets-3.X
cd wxWidgets3.X
mkdir build-rel
cd build-rel
../configure --with-cocoa --prefix="$(pwd)" --enable-debug
make
PATH="$(pwd)":$PATH ; export PATH

## Build erlang

Download erlang R17 (for R16 see below)
cd otp_src_R17XX
otpbuild autoconf (only required when downloading non-release sources)
./configure --enable-darwin-64bit
make
make install (optional, will install to default directory [usually /usr/local])

## R16 Releases

These is a bug in erlang releases prior to R17 which crashes the wx app. If you're using R16 then you will need to perform the following fix:
Delete the line:

  Disconnect andalso (catch wxEvtHandler:disconnect_impl(CbH,O)),

in wxe_server (otp_src_R16XX/lib/wx/src).


LINUX

Ubuntu Trusty

Open terminal, type:

sudo apt-get install build-essential libwxgtk3.0-dev freeglut3-dev libncurses5-dev

# Download/extract latest erlang from http://www.erlang.org/download.html

cd otp_src_***

./configure
make

# Test the installation
cd bin
./erl -smp

# In the erlang shell type:
wx:demo().


Microsoft Windows

Download and install the erlang binary from erlang solutions.
