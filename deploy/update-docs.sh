#!/bin/sh -ex
WWW_REPO=git@github.com:mareframe/mareframe.github.io.git

# If running under travis, go into the build dir
[ -n "${TRAVIS_BUILD_DIR}" ] && cd -- "${TRAVIS_BUILD_DIR}"

# Make sure we are on a stable branch
BRANCH="${TRAVIS_BRANCH}"
[ -n "${TRAVIS_BRANCH}" ] || BRANCH="$(git rev-parse --abbrev-ref HEAD)"
echo Currently on branch ${BRANCH}
echo $BRANCH | grep -q '^[0-9]\.x$' || exit 0

PACKAGE="$(awk '/^Package: / { print $2 }' DESCRIPTION)"
VERSION="$(awk '/^Version: / { print $2 }' DESCRIPTION)"
INST_DIR=/tmp/RPackages
echo Installing package into ${INST_DIR}
mkdir -p $INST_DIR
R_LIBS=$INST_DIR R CMD INSTALL --html ${PACKAGE}_${VERSION}.tar.gz

WWW_DIR=/tmp/gh-pages
echo Cloning into ${WWW_DIR}
[ -d ${WWW_DIR} ] && rm -fr -- "${WWW_DIR}"
git clone ${WWW_REPO} $WWW_DIR

OUT_DIR=${WWW_DIR}/packages/${PACKAGE}/$BRANCH
echo Copying into ${OUT_DIR}
cp -vr ${INST_DIR}/${PACKAGE}/html ${OUT_DIR}
ls -1 ${OUT_DIR}/demo | awk '
BEGIN { print "<html><body><h1>Demos</h1><hr/><ul>"  };
// { print "<ul><a href=\"" $$1 "\">" $$1 "</a></ul>"; };
END { print "</ul></body></html>" }
' > "${OUT_DIR}/demo/index.html"
    
(
    COMMIT="${TRAVIS_COMMIT}"
    [ -n "${COMMIT}" ] || COMMIT="$(git rev-parse HEAD)"
    echo Committing changes
    cd ${WWW_DIR}
    git add -A ${OUT_DIR}
    [ -n "${TRAVIS_BUILD_DIR}" ] && git config user.name "Mareframe CI bot"
    [ -n "${TRAVIS_BUILD_DIR}" ] && git config user.email "jm+mareframe_bot@lentin.co.uk"
    git commit -av -m "Update documentation, mfdb revision $COMMIT"
    git push
)
