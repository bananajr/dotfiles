#!/bin/bash

# first copy everything back to the repo
cat map | awk '!/^[[:space:]]*#/ && !/^[[:space:]]*$/ { print "cp -R " $2 " " $1 }'

