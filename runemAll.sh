#! /bin/bash

for i in PyUtil PyIR PyFort Canon PP; do
    echo ================================= Running $i unit tests =================================
    echo ============================================================================================
    pushd $i/UnitTests
    read
    for t in t_*.py; do
        echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Test $t ^^^^^^^^^^^^^^^^^^^^^^^^^^^
        python $t
        echo ^^^^^^^^^^^^^^^^^^^^^^^^^ End test $t ^^^^^^^^^^^^^^^^^^^^^^^^^
        echo
        read
    done
    popd
    echo =================================== End $i unit tests ===================================
    echo
    echo
done

