#!/usr/bin/env bash
set -e

sh <<< '
    set -e
    sleep 0.2s
    sh <<< "
        sleep 0.2s
        echo TEST-3=\$$
        for i in {1..100}; do
            echo HANG-3 pid=\$$ $i
            sleep 1
        done
    " &
    echo TEST-2=$$
    for i in {1..12}; do
        echo HANG-2 pid=$$ $i
        sleep 1
    done
' &

echo TEST-1=$$
hang() {
    for i in {1..12}; do
        echo HANG-1 pid=$$ $i
        sleep 1
    done
}
hang &
hang
