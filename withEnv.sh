#!/bin/sh
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":$(find ./radare2 -name '*.so' | sed 's|/[^/]*$||' | sort | uniq | sed "s|^\.|$(pwd)|" | tr '\n' ':'):$(pwd)/bin:
$@
