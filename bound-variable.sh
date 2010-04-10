#!/bin/bash

CLASSPATH="classes/:lib/*:src"
exec java -client -cp $CLASSPATH bound_variable.core $*
