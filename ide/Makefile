#!/bin/bash
# ERLC=/usr/local/bin/erlc
# ERLC=/Users/tommo/Documents/Development/erlang/otp_src_R16B03/bin/erlc
ERLC				:= `which erlc`
ERLCFLAGS		= -I ./include -o ./ebin +debug_info
SRCDIR			= src
BEAMDIR			= ./ebin
CP          = cp
APP					= ide.app
OTPAPP			= $(SRCDIR)/$(APP)

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(SRCDIR)/*.erl ;
	@if [ -a $(OTPAPP) ]; then $(CP) $(OTPAPP) $(BEAMDIR)/$(APP); fi; # Copy the application metafile (if exists)
clean: 
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crush.dumpl