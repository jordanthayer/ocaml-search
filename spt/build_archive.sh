#!/bin/bash
#
# This script creates a non-version-controlled release.
#

# Use a directory based on the current date
DIR=spt-`date +%Y-%m-%d`
test -d $DIR && {
    echo "directory ${DIR} already exists"
    exit 1
}

echo "* making the archive in directory ${DIR}"
hg archive ${DIR}

echo "* adding file header comments"
cd ${DIR}
for FILE in $(find . -name \*.ml; find . -name \*.mli)
do
    cat FILE_HEADER ${FILE} > ${FILE}~
    mv ${FILE}~ ${FILE}
done

echo "* removing the FILE_HEADER template from the archive"
rm FILE_HEADER

echo "* remove this script from the archive"
rm build_archive.sh

cd ..
echo "* tar/bz2 the archive"
tar -cjf ${DIR}.tar.bz2 ${DIR}

echo "* tar/gz the archive"
tar -czf ${DIR}.tar.gz ${DIR}

echo "* removing the archive directory."
rm -fr ${DIR}
