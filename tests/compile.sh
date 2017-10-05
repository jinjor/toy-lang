#!/bin/sh
set -eu
cd `dirname $0`

sh ../bin/toy-lang compile ../example/Hello.toy
