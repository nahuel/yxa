#!/bin/sh

set -e

function get_svn
{
    local upversion=$1
    local url=$2
    local filename=$3
    local svnrev=$4

    local srcdir=yxa-${upversion}.orig

    echo Downloading ${filename} from ${url} rev ${svnrev}
    svn export -q -r$svnrev --non-interactive $url ../tarballs/${srcdir}
    echo Autoconf
    (cd ../tarballs/${srcdir} && autoconf)
    echo Clean up snapshot
    rm -rf ../tarballs/${srcdir}/autom4te.cache
    echo Building snapshot tarball
    GZIP=-9 tar -b1 -czf ../tarballs/${filename} -C ../tarballs ${srcdir}
    echo Cleaning up
    rm -rf ../tarballs/${srcdir}
}

function get_http
{
    local url=$1
    local filename=$2

    echo Downloading ${filename} from ${url} ...
    wget -N -nv -T10 -t3 -O ../tarballs/${filename} ${url}
}

function usage
{
    echo "Usage: $0 version url [revision]"
    echo
}

if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Missing parameter"
    usage
    exit 1;
fi

dh_testdir

upversion=$1
url=$2

filename=yxa_${upversion}.orig.tar.gz

if echo $url | grep ^svn ; then
    if [ -z "$3" ]; then
	echo Missing parameter revision
	usage
	exit 1
    fi
    svnrev=$3
    get_svn $upversion $url $filename $svnrev
else
    echo not svn
    get_http $url $filename
fi
