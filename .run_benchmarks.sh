#!/bin/bash

echo "Begin running jenkins benchmark script for Racket typechecking.  First use regression script to build packages:"
set -x
set -e

echo "On linux platforms, check CPU affinity:"
taskset -pc $$ || echo ok

echo "Also check load:"
sar 2 2 || echo ok

echo "And who"
who -a || echo ok

# Switch to the top of the repo:
cd `dirname $0`

NOTEST=1 ./.jenkins_script.sh -j

echo "\nReturned to benchmarking script."

# CONVENTION: The working directory is passed as the first argument.
CHECKOUT=$1
shift || echo ok

if [ "$CHECKOUT" == "" ]; then
  CHECKOUT=`pwd`
fi
if [ "$JENKINS_GHC" == "" ]; then
   echo "JENKINS_GHC unset"
   export JENKINS_GHC=7.8.3
fi
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh
fi
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_cuda.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_cuda.sh
fi

echo "Running benchmarks remotely on server `hostname`"

which cabal
cabal --version

which c2hs     || echo ok
c2hs --version || echo ok

unset GHC
# unset CABAL

set -e

# Switch to where the benchmarks are
# ----------------------------------------
cd "$CHECKOUT"/bench/
# make clean

# Fetch data and build benchmark runner:
make

export TRIALS=3

# Parfunc account LVish uploader project:
# CID=820162629229-kp29aklebt6ucos5a71u8tu3hu8unres.apps.googleusercontent.com
# SEC=pSsMxVAJCFKyWsazuxZVRZwX
# Over limit on [2014.11.12]

if [ "$MACHINECLASS" == "cutter" ]; then
  # Using generic uploader because we're over limit:
  # Generic 1:
  CID=905767673358.apps.googleusercontent.com
  SEC=2a2H57dBggubW1_rqglC7jtK
elif [ "$MACHINECLASS" == "lh008" ]; then 
  # Using generic uploader because we're over limit:
  # Generic 2:
  CID=546809307027-8tm2lp5gtqg5o3pn3s016gd6467cf7j3.apps.googleusercontent.com
  SEC=148aQ08EPpgkb0DiYVLoT9X2
else
  # Generic 3:
  CID=759282369766-ijonhc4662ot2qos4lgud0e0sltjshlj.apps.googleusercontent.com
  SEC=yI8GfZXsHPrW44udqklCHeDH
fi


# [2014.11.10] Moving over to a second table:
TABLENAME=LVish_benchmark_results2
# TABLENAME=LVish_benchmark_results


# LVish table doc ID:  
# TABID=1YxEmNpeUoGCBptDK0ddtomC_oK2IVH1f2M89IIA
# https://www.google.com/fusiontables/DataSource?docid=1YxEmNpeUoGCBptDK0ddtomC_oK2IVH1f2M89IIA

# Enable upload of benchmarking data to a Google Fusion Table:
# ./run-benchmarks-racket-typing.exe --keepgoing --trials=$TRIALS --fusion-upload=$TABID --clientid=$CID --clientsecret=$SEC $*

if [ "$MACHINECLASS" == "" ]; then
    export MACHINECLASS=`hostname -s`
fi

./run-benchmarks-racket-typing.exe --keepgoing --trials=$TRIALS --fusion-upload --name=$TABLENAME --clientid=$CID --clientsecret=$SEC --hostname=$MACHINECLASS $*
