#/bin/bash
ifconfig wlp2s0 | grep "inet " | awk 'BEGIN {FS= " "} {print $2}'
