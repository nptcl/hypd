#!/bin/sh

checkerr()
{
	if [ "$?" -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

aout="a.out"

cd ../example/
checkerr "cd example error"

make clean
checkerr "make clean error"

make
checkerr "make error"

cp ${aout} ../test/.
checkerr "cp error"

make clean
checkerr "make clean error"

echo OK
exit 0

