#!/bin/sh

elm-test &&
sh build.sh &&
sh tests/compile.sh
