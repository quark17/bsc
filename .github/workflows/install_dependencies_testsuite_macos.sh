#!/usr/bin/env bash

brew update

brew install ${BREW_INSTALL_FLAGS} \
  ccache \
  deja-gnu \
  icarus-verilog \
  systemc
