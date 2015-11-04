#!/bin/sh

FLOWC="./flowc"

# Set time limit for all operations
ulimit -t 30

SignalError() {
    if [ $error -eq 0 ] ; then
  echo "FAILED"
  error=1
    fi
    echo "  $1"
}

Run() {
    echo $* 1>&2
    eval $* || {
  SignalError "$1 failed on $*"
  return 1
    }
}

CheckGrammar() {
  echo "#### Testing $1"
  Run "$FLOWC" "-g" "<" $1 
}

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/grammar/test-*.flow"
fi

for file in $files 
do
  CheckGrammar $file 
done

exit $globalerror
