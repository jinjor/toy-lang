#!/bin/sh

sh build.sh &&
elm-test &&
sh tests/compile.sh
