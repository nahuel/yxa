#!/bin/bash

set -e

pkg_name=yxa

function get_svn
{
    local upversion=$1
    local url=$2
    local filename=$3
    local svnrev=$4

    local srcdir=${pkg_name}-${upversion}.orig

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

function get_github
{
    echo get_github
    local upversion=$1
    local url=$2
    local filename=$3
    local rev=$4
    local tmpname=${filename}.tmp
    local gituser=$(echo ${url} | sed -e 's|http://[^/]*/\([^/]*\)/.*|\1|')
    local gitrepo=$(echo ${url} | sed -e 's|http://[^/]*/[^/]*/\([^/]*\)/.*|\1|')
    local revprefix=$(echo ${rev} | sed -e 's/^\(.......\).*$/\1/')
    local srcdir="${gituser}-${gitrepo}-${revprefix}"

    echo Downloading ${filename} from ${url} rev ${rev}
    wget -nv -T10 -t3 -O ../tarballs/${tmpname} ${url}
    (cd ../tarballs && tar xzf ${tmpname})
    echo Autoconf
    (cd ../tarballs/${srcdir} && ./bootstrap)
    echo Clean up snapshot
    rm -rf ../tarballs/${srcdir}/autom4te.cache
    echo Building snapshot tarball
    GZIP=-9 tar -b1 -czf ../tarballs/${filename} -C ../tarballs ${srcdir}
    echo Cleaning up
    rm -rf ../tarballs/${srcdir} ../tarballs/${tmpname}
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

filename=${pkg_name}_${upversion}.orig.tar.gz

if echo $url | grep ^svn ; then
    if [ -z "$3" ]; then
	echo Missing parameter revision
	usage
	exit 1
    fi
    svnrev=$3
    get_svn $upversion $url $filename $svnrev
elif echo $url | grep github.com ; then
    if [ -z "$3" ]; then
	echo Missing parameter revision
	usage
	exit 1
    fi
    rev=$3
    get_github $upversion $url $filename $rev
else
    echo not svn
    get_http $url $filename
fi
