#!/bin/sh
set -e
cd `dirname $0`

sh ../bin/toy-lang compile ../example/Hello.toy
sh ../bin/toy-lang compile -c "a = 1" 1> /dev/null
