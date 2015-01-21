#!/bin/bash

set -x 
set -e

TOP=`pwd`
if [ "$CABAL" == "" ]; then 
  CABAL=cabal
fi 
if [ "$GHC" == "" ]; then 
  GHC=ghc
fi 
if [ "$SHOWDETAILS" == "" ]; then 
  SHOWDETAILS=always
fi
if [ "$MAX_CPUS" == "" ]; then
    MAX_CPUS=4
fi

# ----------------------------------------
# STEP (3): build input data:

  # Before doing anything else, make sure we've built our test input files:
  (cd data; make)

# ----------------------------------------
# STEP (4): Run Tests

  # Test only the top-level directory:
  $CABAL install   --enable-tests --only-dep $CBLEXTRAS
  $CABAL configure --enable-tests --with-ghc=$GHC
  # $CABAL test --show-details=$SHOWDETAILS --test-options="--hide-successes -j16 --timeout=5 +RTS -N -s" 

  $CABAL test --show-details=$SHOWDETAILS --test-options="-j1 --timeout=5 --maximum-test-size=1 +RTS -N1 -s"  testSubtype
  $CABAL test --show-details=$SHOWDETAILS --test-options="-j$MAX_CPUS --timeout=5 --hide-successes +RTS -N$MAX_CPUS -s" testSubtypeStored
  $CABAL test --show-details=$SHOWDETAILS --test-options="-j1 --timeout=5 +RTS -N1 -s"  testInferSeq

  # Disabling this until it works fully:
  #  $CABAL test --show-details=$SHOWDETAILS --test-options="-j1 --timeout=5 +RTS -N1 -s"  testInfer

  # $CABAL install --enable-tests --show-details=$SHOWDETAILS --with-ghc=$GHC

# Finally, make sure the benchmark stuff at least builds:
cd $TOP/bench
make

