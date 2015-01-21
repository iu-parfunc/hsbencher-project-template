#!/bin/bash

# NOTE: This script responds to additional env vars.  Here are some examples:
# 
#  * JENKINS_GHC=7.6.3 -- set the GHC version
#  * JENKINS_GHC_OPTS  -- optionally supply an extra argument to GHC; one token only
#  * CABAL_FLAGS1      -- set by jenkins, passed thru to cabal install
#  * CABAL_FLAGS2      -- set by jenkins, passed thru to cabal install
#  * CABAL_FLAGS3      -- set by jenkins, passed thru to cabal install
#  * CABAL             -- if set, control which cabal to use
#  * PROF=1            -- turn on library profiling during installation
#  * NOSANDBOX=1       -- Skip the sandbox creation step 
#  * NOTEST=1          -- Skip the testing step.  Just build.
#
# This script also passes through extra command args to the cabal
# install commands it issues.

export CBLEXTRAS=$*

echo LD_LIBRARY_PATH: $LD_LIBRARY_PATH
echo LIBRARY_PATH: $LIBRARY_PATH

set -x
# WHICHDRIFT=`which drift-ghc`
set -e

if [ "$CABAL" == "" ]; then 
  CABAL=cabal
fi 

which -a $CABAL

if [ "$JENKINS_GHC" == "" ]; then 
  GHC=ghc
else
  ENVSCRIPT=$HOME/rn_jenkins_scripts/acquire_ghc.sh
  # This is specific to our testing setup at IU:
  if [ -f "$ENVSCRIPT" ]; then 
    source "$ENVSCRIPT"
  fi
  GHC=ghc-$JENKINS_GHC
fi

if [ "$JENKINS_GHC_OPTS" != "" ]; then
    # WARNING: Has to be a single token:
    CBLEXTRAS="$CBLEXTRAS --ghc-options=$JENKINS_GHC_OPTS"
fi 

# Additional packages:
PKGS=" ./lvars/haskell/lvish ./lvars/haskell/par-classes ./lvars/haskell/par-collections "
MORESANDBOXES=" ./bench ./bench/BoolVarTree/ ./bench/TRinfer-bigcall ./bench/TRinfer-treecall ./hm-typing/ "
# ./lvars/haskell/par-transformers  

# ----------------------------------------
# STEP (1): Sandbox init.

TOP=`pwd`
if [ "$NOSANDBOX" == "" ]; then
  $CABAL sandbox init
  $CABAL sandbox hc-pkg list
  for path in $PKGS $MORESANDBOXES; do 
    cd $TOP/$path
    $CABAL sandbox init --sandbox=$TOP/.cabal-sandbox
  done
fi

# Add the current directory in:
ALLPKGS="$PKGS ./"
cd $TOP

# -fgeneric flag is for LVish 
CFG=" --disable-documentation -fgeneric -fchaselev -f-getonce "

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then 
  CFG="$CFG --disable-library-profiling --disable-executable-profiling"
else
  CFG="$CFG --enable-library-profiling --enable-executable-profiling"
fi  
CABAL_FLAGS="$CABAL_FLAGS1 $CABAL_FLAGS2 $CABAL_FLAGS3"

# Wrinkle: this package needs DrIFT before it will build:
# if [ "" == "$WHICHDRIFT" ]; then
#   export PATH=$PATH:$TOP/.cabal-sandbox/bin/
#   $CABAL install DrIFT
# fi

# ----------------------------------------
# STEP (2): Install packages

# Avoiding this because of complicated deps problems with some of those other modules's test suites:
# $CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC $ALLPKGS --enable-tests --only-dep $*
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC $ALLPKGS --force-reinstalls $CBLEXTRAS

# ----------------------------------------
# STEP (3): Regression Testing

if [ "$NOTEST" == "" ]; then 
  export CABAL
  ./.run_tests.sh
fi
