#!/bin/bash

CLASS=$1
OPTIONS=${@:2}

sbt $OPTIONS "testOnly $CLASS"

