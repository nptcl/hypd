#!/bin/sh

checkerr()
{
	if [ "$?" -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

input="$HOME/work/debug/npt.tar"
encode="./debug.encode"
decode="./debug.decode"
log="./debug.log"
aout="a.out"
zlib="zlib"

[ -x ${aout} ]
checkerr "${aout} error"

[ -x ${zlib} ]
checkerr "${zlib} error"

[ -r ${input} ]
checkerr "debug input error"


##
##  deflate  (raw mode)
##
deflate_self_self()
{
	echo "deflate.encode: a.out -> a.out"
	./${aout} -r -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "deflate.decode: a.out -> a.out"
	./${aout} -r -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "deflate.md5: OK,  [${md5_x}]"
}


##
##  zlib
##
zlib_self_self()
{
	echo "zlib.encode: a.out -> a.out"
	./${aout} -z -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "zlib.decode: a.out -> a.out"
	./${aout} -z -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "zlib.md5: OK,  [${md5_x}]"
}

zlib_self_zlib()
{
	echo "zlib.encode: a.out -> zlib"
	./${aout} -z -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "zlib.decode: a.out -> zlib"
	./${zlib} -z -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "zlib.md5: OK,  [${md5_x}]"
}

zlib_zlib_self()
{
	echo "zlib.encode: a.out -> self"
	./${zlib} -z -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "zlib.decode: a.out -> self"
	./${aout} -z -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "zlib.md5: OK,  [${md5_x}]"
}

zlib_zlib_self_zero()
{
	echo "zlib.encode: a.out -> self"
	./${zlib} -z -e0 ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "zlib.decode: a.out -> self"
	./${aout} -z -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "zlib.md5: OK,  [${md5_x}]"
}


##
##  gzip
##
gzip_self_self()
{
	echo "gzip.encode: a.out -> a.out"
	./${aout} -g -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "gzip.decode: a.out -> a.out"
	./${aout} -g -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "gzip.md5: OK,  [${md5_x}]"
}

gzip_self_gzip()
{
	echo "gzip.encode: a.out -> gzip"
	./${aout} -g -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "gzip.decode: a.out -> gzip"
	./${zlib} -g -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "gzip.md5: OK,  [${md5_x}]"
}

gzip_gzip_self()
{
	echo "gzip.encode: a.out -> self"
	./${zlib} -g -e ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "gzip.decode: a.out -> self"
	./${aout} -g -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "gzip.md5: OK,  [${md5_x}]"
}

gzip_gzip_self_zero()
{
	echo "gzip.encode: a.out -> self"
	./${zlib} -g -e0 ${input} ${encode} > ${log} 2>&1
	checkerr "a.out error"

	echo "gzip.decode: a.out -> self"
	./${aout} -g -d ${encode} ${decode} > ${log} 2>&1
	checkerr "a.out error"

	md5_x="$(md5 < ${input})"
	checkerr "md5_x error"
	md5_y="$(md5 < ${decode})"
	checkerr "md5_y error"
	[ -n ${md5_x} ]
	checkerr "md5_x string error"
	[ "${md5_x}" == "${md5_y}" ]
	checkerr "md5 error"
	echo "gzip.md5: OK,  [${md5_x}]"
}


deflate_self_self
zlib_self_self
zlib_self_zlib
zlib_zlib_self
zlib_zlib_self_zero
gzip_self_self
gzip_self_gzip
gzip_gzip_self
gzip_gzip_self_zero

exit 0

