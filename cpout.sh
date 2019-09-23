#!/bin/bash

# copies everything from the repo into their designated places
cat map | awk '!/^[[:space:]]*#/ && !/^[[:space:]]*$/ { print "cp -RTv " $1 " " $2 }' | bash
