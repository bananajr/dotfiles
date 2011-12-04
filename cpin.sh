#!/bin/bash

# copies everything back into the repo before pulling down changes from origin
cat map | awk '!/^[[:space:]]*#/ && !/^[[:space:]]*$/ { print "cp -R " $2 " " $1 }' | bash

