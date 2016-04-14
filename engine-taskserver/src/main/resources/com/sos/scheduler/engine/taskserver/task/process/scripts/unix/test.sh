#! /bin/sh
set -e

sh <<< '
    set -e
    sh -c "echo TEST-3=\$$; ping -c 100 127.0.0.1" &
    echo TEST-2=$$
    ping -c 100 127.0.0.1
' &

echo TEST-1=$$
ping -c 100 127.0.0.1 &
ping -c 100 127.0.0.1
