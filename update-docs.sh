#!/bin/sh -e
WWW_REPO=git@github.com:mareframe/mareframe.github.io.git

# Make sure we are on a stable branch
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
echo $BRANCH | grep -q '^[0-9]\.x$' || exit 0

# Install package, building HTML
PACKAGE="$(awk '/^Package: / { print $2 }' DESCRIPTION)"
VERSION="$(awk '/^Version: / { print $2 }' DESCRIPTION)"
INST_DIR=/tmp/RPackages
mkdir -p $INST_DIR
R_LIBS=$INST_DIR R CMD INSTALL --html ${PACKAGE}_${VERSION}.tar.gz

# Clone docs, copy HTML
WWW_DIR=/tmp/gh-pages
[ -d ${WWW_DIR} ] && rm -fr -- "${WWW_DIR}"
OUT_DIR=${WWW_DIR}/packages/${PACKAGE}/$BRANCH
git clone ${WWW_REPO} $WWW_DIR
cp -vr ${INST_DIR}/${PACKAGE}/html ${OUT_DIR}
ls -1 ${OUT_DIR}/demo | awk '
BEGIN { print "<html><body><h1>Demos</h1><hr/><ul>"  };
// { print "<ul><a href=\"" $$1 "\">" $$1 "</a></ul>"; };
END { print "</ul></body></html>" }' \
    > "${OUT_DIR}/demo/index.html"

# Commit changes to docs
(
    cd ${WWW_DIR}
    git add -A ${OUT_DIR}
    git commit -av -m "Update documentation, mfdb revision "
)
