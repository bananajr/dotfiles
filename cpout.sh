#!/bin/bash

# copies everything from the repo into their designated places
cat map | awk '!/^[[:space:]]*#/ && !/^[[:space:]]*$/ { print "cp -R " $1 " " $2 }' | bash
