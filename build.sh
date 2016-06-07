#!/bin/bash -e
set -x

for param in "$@"
	do case $param in
		--publish*)
			publish="1"
		;;
		--patch-version=*)
			patch_version="${param#*=}"
		;;
	esac
done

if [ -n "$publish" ] ; then
	sbt ';set version <<= (version)(_ + ".'${patch_version:-SNAPSHOT}'")' clean test publish
fi
