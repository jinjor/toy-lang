#!/bin/sh
set -e
cd `dirname $0`

sh ../bin/toy-lang compile ../example/Main.toy
sh ../bin/toy-lang compile ../example/Valid.toy -o ../tmp/index.js
sh ../bin/toy-lang compile -c "a = 1" 1> /dev/null
