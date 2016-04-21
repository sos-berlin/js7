#! /bin/sh
set -e

sh <<< '
    set -e
    sh <<< "
        echo TEST-3=\$$
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
        echo HANG-3 pid=\$$; sleep 1
    " &
    echo TEST-2=$$
    n=12
    while [ $n -gt 0 ]; do
        n=`expr $n - 1`
        echo HANG-2 pid=$$ $n
        sleep 1
    done
' &

echo TEST-1=$$
hang() {
    n=12
    while [ $n -gt 0 ]; do
        n=`expr $n - 1`
        echo HANG-1 pid=$$ $n
        sleep 1
    done
}
hang &
hang
